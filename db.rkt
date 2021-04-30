;; Copyright (C) 2021 by Greg Hendershott

#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (prefix-in db: db)
         openssl/sha1
         syntax/parse/define
         racket/async-channel
         racket/contract
         racket/file
         racket/list
         racket/match
         racket/set
         sql
         "common.rkt")

(provide create-database
         create-tables

         open
         close
         start-analyze-more-files-thread
         stop-analyze-more-files-thread
         analyze-path

         add-def
         add-arrow
         add-rename
         add-import
         add-export
         add-mouse-over-status
         add-tail-arrow
         add-unused-require
         queue-more-files-to-analyze
         analyze-all-known-paths

         ;; High level queries
         use-pos->def
         use-pos->def/transitive
         def-pos->uses
         def-pos->uses/transitive
         find-def
         find-all-defs-named

         use-pos->name

         ;; Low level queries. These are like the same-named `db`
         ;; functions, but instead of supply the connection as the
         ;; first argument, the connection created by `open` is used.
         query
         query-exec
         query-rows
         query-maybe-row
         query-maybe-value
         query-list
         intern)

(define current-dbc (make-parameter #f))

(define-syntax-parser define-db
  [(_ id:id)
   #:with real-id (format-id #'id "db:~a" #'id)
   #'(define (id . args) (apply real-id (current-dbc) args))])
(define-db query)
(define-db query-exec)
(define-db query-rows)
(define-db query-maybe-row)
(define-db query-maybe-value)
(define-db query-list)

(define/contract (open what [analyze-code-proc void])
  (->* ((or/c 'memory 'temporary path-string?))
       ((-> path-string? string? any))
       any)
  (current-analyze-code analyze-code-proc)
  (current-dbc (db:sqlite3-connect #:database  what
                                   #:mode      'read/write
                                   #:use-place (path-string? what)))
  ;; Enforce foreign key constraints.
  ;; <https://sqlite.org/pragma.html#pragma_foreign_keys>
  (query-exec "pragma foreign_keys = on")
  ;; "With synchronous OFF (0), SQLite continues without syncing as
  ;; soon as it has handed data off to the operating system. If the
  ;; application running SQLite crashes, the data will be safe, but
  ;; the database might become corrupted if the operating system
  ;; crashes or the computer loses power before that data has been
  ;; written to the disk surface. On the other hand, commits can be
  ;; orders of magnitude faster with synchronous OFF."
  ;; <https://sqlite.org/pragma.html#pragma_synchronous>
  (query-exec "pragma synchronous = off")
  ;; "The WAL journaling mode uses a write-ahead log instead of a
  ;; rollback journal to implement transactions. The WAL journaling
  ;; mode is persistent; after being set it stays in effect across
  ;; multiple database connections and after closing and reopening the
  ;; database. A database in WAL journaling mode can only be accessed
  ;; by SQLite version 3.7.0 (2010-07-21) or later."
  ;; <https://sqlite.org/pragma.html#pragma_journal_mode>
  (query-exec "pragma journal_mode = WAL")
  (void))

(define (close)
  (stop-analyze-more-files-thread)
  (define dbc (current-dbc))
  (current-dbc #f)
  (when (and (db:connection? dbc)
             (db:connected? dbc))
    (db:disconnect dbc)))

(define sema (make-semaphore 1))
(define current-analyze-code (make-parameter void))
(define (analyze-path path #:always? [always? #f])
  (when (equal? void (current-analyze-code))
    (error 'analyze-path "open was not called with a non-void `analyze-code` argument.\n You may call functions that query the db."))
  (call-with-semaphore
   sema
   (λ ()
     (when always? (forget-digest path))
     (define code-str (file->string path #:mode 'text))
     (define digest (sha1 (open-input-string code-str)))
     (and (update-digest path digest)
          (with-time/log (format "analyze ~v" (str path))
            (delete-tables-involving-path path)
            (with-handlers ([exn:fail?
                             (λ (e)
                               (log-pdb-error "error analyzing ~v: ~v" path (exn-message e))
                               (delete-tables-involving-path path)
                               (forget-digest path)
                               #f)])
              ((current-analyze-code) path code-str)
              #t))))))

(define (create-tables)
  ;; The `strings` table is like interned symbols. There are many
  ;; long, repeated strings -- such as for paths, submods,
  ;; identifiers. These can be replaced by integer foreign keys.
  ;;
  ;; Saves much space. Also speeds queries that test for equality.
  ;; Although it complicates some queries that need the string values,
  ;; by requiring table join(s), the "ON" clauses are cheap integer
  ;; equality.
  ;;
  ;; (Although this might seem similar also to a "snowflake schema" in
  ;; a data warehouse, strictly that would be if we had a table for
  ;; each kind of thing a.k.a. "dimension". Instead this is one shared
  ;; table; really just interning. Even so, it does makes tables like
  ;; `arrows` and `defs` look a lot like data warehouse "fact" tables,
  ;; whose columns are all integers, many of which are foreign keys
  ;; into `strings`.)
  (query-exec
   (create-table
    #:if-not-exists strings
    #:columns
    [id          integer #:not-null]
    [str         string  #:not-null]
    #:constraints
    (primary-key id)
    (unique      str)))

  ;; This table of sha-1 digests for paths is how we record whether to
  ;; bother re-analyzing a file.
  (query-exec
   (create-table
    #:if-not-exists digests
    #:columns
    [path        integer #:not-null]
    [digest      string  #:not-null]
    #:constraints
    (primary-key path)
    (foreign-key path #:references (strings id))))

  ;; A table of arrows, both lexical and imported, as reported by
  ;; syncheck:add-arrow. In addition when the definition is imported
  ;; -- when `kind` is not "lexical" -- it also includes information
  ;; from identifier-binding -- a superset of what
  ;; syncheck:add-jump-to-definition supplies -- about the modpath
  ;; where it is defined. The precise location within that other file
  ;; can be found in `defs` after that file is also analyzed.
  (query-exec
   (create-table
    #:if-not-exists arrows
    #:columns
    [use_path    integer #:not-null]
    [use_beg     integer #:not-null]
    [use_end     integer #:not-null]
    ;; `use_text` is the annotated text at the use site, i.e. it is
    ;; the [use_beg use_end) interval of use_path. It can be a
    ;; substring of the `use_stx` column, in the case of multiple
    ;; arrows for sub-sections of one identifier, arising from e.g.
    ;; prefix-in or the 'sub-range-binders syntax property.
    [use_text    integer #:not-null]
    [use_stx     integer #:not-null]
    ;; One of {"lexical" "require" "module-lang"}
    [kind        integer #:not-null]
    ;; When `kind` is 0 ("lexical"), this is the local definition
    ;; site. Otherwise, this is the require site.
    [def_beg     integer #:not-null]
    [def_end     integer #:not-null]
    [def_text    integer #:not-null] ;text at def site
    [def_stx     integer #:not-null] ;is this ever useful??
    ;; Unless kind is 0 ("lexical"), these correspond to
    ;; identifier-binding from-xxx items. Specifically, join these on
    ;; the `defs` table to find the location within the file, if
    ;; already known. When kind="lexical", only from_path is
    ;; meaningful and is simply the same as use_path.
    [from_path   integer] ;from-mod
    [from_subs   integer] ;from-mod
    [from_id     integer] ;from-sym
    ;; Unless `kind` is0 ("lexical"), these correspond to
    ;; identifier-binding nominal-from-xxx items:
    [nom_path    integer] ;nominal-from-mod
    [nom_subs    integer] ;nominal-from-mod
    [nom_id      integer] ;nominal-sym
    #:constraints
    (primary-key use_path use_beg use_end)
    (foreign-key use_path #:references (strings id))
    (foreign-key use_text #:references (strings id))
    (foreign-key use_stx #:references (strings id))
    (foreign-key def_text #:references (strings id))
    (foreign-key def_stx #:references (strings id))
    (foreign-key from_path #:references (strings id))
    (foreign-key from_subs #:references (strings id))
    (foreign-key from_id #:references (strings id))
    (foreign-key nom_path #:references (strings id))
    (foreign-key nom_subs #:references (strings id))
    (foreign-key nom_id #:references (strings id))))
  ;; TODO: Add indexes, e.g. for [def_beg def_end] columns?


  ;; A table of definitions in files, as reported by
  ;; syncheck:add-definition-target. Note that this does not include
  ;; sites of lexical definitions.
  (query-exec
   (create-table
    #:if-not-exists defs
    #:columns
    ;; Each definition is uniquely identified by -- i.e. the primary
    ;; key is -- the triple (path subs sym).
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [sym         integer #:not-null]
    ;; Otherwise we just record the [beg end) location within the
    ;; file.
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key path subs sym)
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key sym #:references (strings id))))

  ;; This view abstracts over the difference between arrows for
  ;; lexical definitions and arrows for imported definitions. It left
  ;; joins `arrows` on the `defs` table for imports; note that def_beg
  ;; and def_end may be sql-null when the defining file has not yet
  ;; been analyzed.
  (query-exec
   (create-view
    xrefs
    (select
     arrows.use_path
     arrows.use_beg
     arrows.use_end
     arrows.use_text
     arrows.use_stx
     (as (case #:of kind [0 arrows.use_path] [else arrows.from_path]) def_path)
     (as (case #:of kind [0 arrows.def_beg]  [else defs.beg])         def_beg)
     (as (case #:of kind [0 arrows.def_end]  [else defs.end])         def_end)
     (as (case #:of kind [0 arrows.def_text] [else arrows.from_id])   def_text)
     #:from (left-join
             arrows defs
             #:on
             (and (= arrows.from_path defs.path)
                  (= arrows.from_subs defs.subs)
                  (= arrows.from_id   defs.sym))))))

  ;; A table of imports. This is useful for completion candidates --
  ;; symbols that could be used, even if they're not yet (and
  ;; therefore don't have any arrow).
  (query-exec
   (create-table
    #:if-not-exists imports
    #:columns
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [sym         integer #:not-null]
    #:constraints
    (primary-key path subs sym)
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key sym #:references (strings id))))

  ;; A table of syncheck:add-mouse-over-status annotations
  (query-exec
   (create-table
    #:if-not-exists mouseovers
    #:columns
    [path        integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    [text        integer #:not-null]
    #:constraints
    (foreign-key path #:references (strings id))
    (foreign-key text #:references (strings id))
    (unique      path beg end text)))

  ;; A table of syncheck:add-tail-arrow annotations
  (query-exec
   (create-table
    #:if-not-exists tail_arrows
    #:columns
    [path        integer #:not-null]
    [tail        integer #:not-null]
    [head        integer #:not-null]
    #:constraints
    (foreign-key path #:references (strings id))
    (unique      path tail head)))

  ;; A table of syncheck:add-unused-require annotations
  (query-exec
   (create-table
    #:if-not-exists unused_requires
    #:columns
    [path        integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (foreign-key path #:references (strings id))
    (unique      path beg end)))

  ;; Given some use, to find the set of same-named sites across 1 or
  ;; more files, we need traverse a different graph than the graph for
  ;; definitions.
  ;;
  ;; 0. A lexical arrow: The definition site is the final set member.
  ;;
  ;; 1. An import.
  ;;    - If it renames, the new name is the final set member.
  ;;    - Else continue following the graph with {nom-path
  ;;      nom-submods nom-id}.
  ;;
  ;; 2. An export:
  ;;    - If it renames, the new name is the final set member.
  ;;    - Else continue folloiwng the graph.

  ;; Like xrefs, but uses nominal-{path subs id}. Therefore more
  ;; granular, there will be a step for each renaming export or
  ;; import. This /doesn't/ handle arrows between old and names
  ;; /within/ each renaming provide or require.
  (query-exec
   (create-view
    noms
    (select
     arrows.use_path
     arrows.use_beg
     arrows.use_end
     arrows.use_text
     arrows.use_stx
     (as (case #:of arrows.kind [0 arrows.use_path] [else arrows.nom_path]) nom_path)
     (as (case #:of arrows.kind [0 arrows.def_text] [else arrows.nom_id])   nom_id)
     rhs.def_beg
     rhs.def_end
     #:from (left-join
             arrows (as arrows rhs)
             #:on (and (= arrows.nom_path rhs.use_path)
                       (= arrows.nom_subs rhs.nom_subs)
                       (= arrows.nom_id   rhs.nom_id))))))

  (query-exec
   (create-view
    renaming_exports
    (select (as arrows.use_path use_path)
            (as arrows.use_beg use_beg)
            (as arrows.use_end use_end)
            (as new_to_old.use_beg def_beg)
            (as new_to_old.use_end def_end)
            (as new_to_old.use_text use_text)
            #:from
            (inner-join
             arrows (as arrows new_to_old)
             #:on (and
                   ;;(= new_to_old.kind 0)
                   ;; Both point to the same location
                   (= arrows.nom_path new_to_old.use_path)
                   (= arrows.def_beg new_to_old.def_beg)
                   (= arrows.def_end new_to_old.def_end)
                   ;; Both have the same use_text e.g. `new`
                   (= arrows.nom_id new_to_old.nom_id))))))

  ;; Given
  ;;
  ;;     (rename-in "file.rkt" [old new])
  ;;     new
  ;;
  ;; or
  ;;
  ;;     (only-in "file.rkt" [old new])
  ;;     new
  ;;
  ;; there will exist in the `arrows` table the following arrows:
  ;;
  ;; 1. A require arrow from the use of `new` to "file.rkt".
  ;;
  ;; 2. A lexical arrow from `old` to "file.rkt" within the require.
  ;;
  ;; 3. A lexical arrow from 'new' to 'old' within the require.
  ;;
  ;; But no arrow exists, directly from a use of `new` to the `new`
  ;; stx within the require.
  ;;
  ;; Such a 4th arrow is implied by the other 3, as expressed by this
  ;; view.
  (query-exec
   (create-view
    renaming_imports
    (select (as arrows.use_path use_path)
            (as arrows.use_beg use_beg)
            (as arrows.use_end use_end)
            (as new_to_file.use_beg def_beg)
            (as new_to_file.use_end def_end)
            (as new_to_file.use_text use_text)
            arrows.kind
            #:from
            (inner-join
             arrows
             ;; These are implied arrows from the `new` name to the
             ;; modpath within the renaming require. i.e. Collapse
             ;; arrows 2 and 3 to a single transitive arrow.
             (as (select
                  (as old_to_file.use_path use_path)
                  (as new_to_old.use_beg use_beg)
                  (as new_to_old.use_end use_end)
                  (as old_to_file.def_beg def_beg)
                  (as old_to_file.def_end def_end)
                  (as new_to_old.use_text use_text)
                  #:from
                  (inner-join
                   (as arrows new_to_old)
                   (as arrows old_to_file)
                   #:on (and (= new_to_old.use_path old_to_file.use_path)
                             (= new_to_old.def_beg old_to_file.use_beg))))
                 new_to_file)
             #:on (and
                   ;; require arrow (not lexical or module-lang)
                   (= arrows.kind 1)
                   ;; Both point to the same location e.g. "file.rkt"
                   (= arrows.use_path new_to_file.use_path)
                   (= arrows.def_beg new_to_file.def_beg)
                   (= arrows.def_end new_to_file.def_end)
                   ;; Both have the same use_text e.g. `new`
                   (= arrows.use_text new_to_file.use_text))))))
  ;; One way to find /uses/ of renamed imports: The use site text
  ;; doesn't match the nom-id. (However, ignore prefix-in sites, b/c
  ;; check-syntax gives us 2 arrows for those. The prefix is N/A. The
  ;; suffix, if it matches nom-id, is /not/ from a rename-in.)
  ;;
  ;; (query
  ;;  (select *
  ;;          #:from ArrowsView
  ;;          #:where
  ;;          (and (<> nom_id use_text)
  ;;               (<> (|| use_text nom_id) use_stx))))


  ;;; Optional convenience views
  ;;;
  ;;; These aren't necessarily efficient; not intended for "real" use.
  ;;; However they have some documentation value, and, they can be
  ;;; handy when debugging, to explore seeing "de-interned", stringy
  ;;; values.

  (query-exec
   (create-view
    ArrowsView
    (select
     (as (select str #:from strings #:where (= strings.id use_path)) use_path)
     use_beg
     use_end
     (as (select str #:from strings #:where (= strings.id use_text)) use_text)
     (as (select str #:from strings #:where (= strings.id use_stx))  use_stx)
     (as (case #:of kind [0 "lexical"] [1 "require"] [2 "module-lang"] [else "!!!"]) kind)
     def_beg
     def_end
     (as (select str #:from strings #:where (= strings.id def_text))  def_text)
     (as (select str #:from strings #:where (= strings.id def_stx))   def_stx)
     (as (select str #:from strings #:where (= strings.id from_path)) from_path)
     (as (select str #:from strings #:where (= strings.id from_subs)) from_subs)
     (as (select str #:from strings #:where (= strings.id from_id))   from_id)
     (as (select str #:from strings #:where (= strings.id nom_path))  nom_path)
     (as (select str #:from strings #:where (= strings.id nom_subs))  nom_subs)
     (as (select str #:from strings #:where (= strings.id nom_id))    nom_id)
     #:from arrows)))

  (query-exec
   (create-view
    DefsView
    (select
     (as (select str #:from strings #:where (= strings.id path)) path)
     (as (select str #:from strings #:where (= strings.id subs)) subs)
     (as (select str #:from strings #:where (= strings.id sym))  sym)
     beg
     end
     #:from defs))))

(define/contract (create-database path)
  (-> path-string? any)
  (unless (file-exists? path)
    (log-pdb-warning "~v does not exist; creating it and tables" path)
    (parameterize ([current-dbc (db:sqlite3-connect #:database  path
                                                    #:mode      'create
                                                    #:use-place #f)])
      (create-tables)
      (db:disconnect (current-dbc)))))

;; This applies a value to `str`, ensures it's in the `strings` table,
;; and returns the id. We use this extensively for paths, symbols, and
;; strings.
(define intern
  (let ([ht (make-hash)])
    (λ (v)
      (hash-ref!
       ht v
       (λ ()
         (define s (str v))
         ;; I don't know how to do this as a single query in sqlite.
         ;; IIUC these two queries needn't be wrapped in a transaction
         ;; because we never delete things from the strings table.
         (query-exec (insert #:into strings #:set [str ,s] #:or-ignore))
         (query-maybe-value (select id #:from strings #:where (= str ,s))))))))

;; Ensure there is a row in the digests table for path and digest.
;; Returns #f when a row already existed and already had the digest.
;; IOW returns true for a new or changed file, and the caller might
;; want to (re)do something.
(define (update-digest path actual-digest)
  ;; FIXME? Need to use a transaction here?
  (define pathid (intern path))
  (match (query-maybe-value
          (select digest #:from digests #:where (= path ,pathid)))
    [(== actual-digest)
     #f]
    [#f
     (query-exec
      (insert #:into digests #:set [path ,pathid] [digest ,actual-digest]))
     #t]
    [_other-digest
     (query-exec
      (update digests #:set [digest ,actual-digest] #:where (= path ,pathid)))
     #t]))

;; Remove a digest. This is a way to force things like `analyze` to
;; redo work.
(define (forget-digest path)
  (query-exec
   (delete #:from digests #:where (= path ,(intern path)))))

;; This will only re-analyze a path when its digest is out-of-date --
;; or when always? is true, in which case all known files are analyzed.
;; Either way, the usual behavior of analyze-path applies: When
;; analyzing a file discovers dependencies on other files, it queues
;; those to be analyzed, too.
(define (analyze-all-known-paths #:always? [always? #f])
  (for ([path (in-list
               (query-list
                (select str #:from (inner-join
                                    digests strings
                                    #:on (= digests.path strings.id)))))])
    (analyze-path path #:always? always?)))

(define (delete-tables-involving-path path)
  (define pathid (intern path))
  (query-exec (delete #:from arrows   #:where (= use_path ,pathid)))
  (query-exec (delete #:from defs   #:where (= path    ,pathid))))

(define (add-def path beg end subs symbol)
  (query-exec
   (insert #:into defs #:set
           [path ,(intern path)]
           [subs ,(intern subs)]
           [sym  ,(intern symbol)]
           [beg  ,beg]
           [end  ,end]
           ;; FIXME: check-syntax will report identical
           ;; (path symbol subs) for a file-module-level
           ;; define and one inside a module+ form. :( I'd
           ;; expect subs to include the module+ name
           ;; but it does not. So for now simply ignore
           ;; any such shadowing definitions.
           #:or-ignore)))

(define (add-arrow use-path
                   use-beg use-end use-text use-stx
                   require-arrow
                   def-beg def-end def-text def-stx
                   [from-path #f] [from-subs #f] [from-id #f]
                   [nom-path #f]  [nom-subs #f]  [nom-id #f])
  (define (intern/null v)
    (if v (intern v) db:sql-null))
  (define kind (match require-arrow [#f 0] [#t 1] ['module-lang 2]))
  (query-exec
   (insert #:into arrows #:set
           [use_path  ,(intern use-path)]
           [use_beg   ,use-beg]
           [use_end   ,use-end]
           [use_text  ,(intern use-text)]
           [use_stx   ,(intern use-stx)]
           [kind      ,kind]
           [def_beg   ,def-beg]
           [def_end   ,def-end]
           [def_text  ,(intern def-text)]
           [def_stx   ,(intern def-stx)]
           [from_path ,(intern/null from-path)]
           [from_subs ,(intern/null from-subs)]
           [from_id   ,(intern/null from-id)]
           [nom_path  ,(intern/null nom-path)]
           [nom_subs  ,(intern/null nom-subs)]
           [nom_id    ,(intern/null nom-id)]
           ;; For things like `struct`, check-syntax might duplicate
           ;; syncheck:add-jump-to-definition.
           #:or-ignore)))

(define (add-rename path subs old-stx new-stx kind path-stx)
  ;; Say that the rename is an additional use of the originally
  ;; defined thing.
  #;(println (list 'add-rename #;path subs old-stx new-stx kind path-stx))
  (define (stx->vals stx)
    (define dat (syntax-e stx))
    (define beg (syntax-position stx))
    (define span (syntax-span stx))
    (define end (and beg span (+ beg span)))
    (values dat beg end))
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and new-beg new-end)
    (add-arrow path
               new-beg new-end new-sym new-sym
               #f ;lexical
               (or old-beg new-beg) (or old-end new-end) old-sym old-sym
               path subs old-sym
               path subs new-sym))
  (when (eq? kind 'import)
    ;; Also add arrow from the old name to the mod path
    (define-values (path-sym path-beg path-end) (stx->vals path-stx))
    (when (and old-beg old-end path-beg path-end)
      (define-values (from-path from-submods from-sym nom-path nom-submods nom-sym)
        (identifier-binding/resolved path old-stx 0 (syntax->datum old-stx)))
      (add-arrow path
                 old-beg old-end old-sym old-sym
                 #t ;require
                 path-beg path-end path-sym path-sym
                 from-path from-submods from-sym
                 nom-path nom-submods nom-sym))))

(define (add-import path subs sym)
  (void)
  #;
  (query-exec
   (insert #:into imports #:set
           [path ,(intern path)]
           [subs ,(intern subs)]
           [sym  ,(intern sym)]
           #:or-ignore)))

(define (add-export path subs sym)
  (void))

(define (add-mouse-over-status path beg end text)
  (query-exec
   (insert #:into mouseovers #:set
           [path ,(intern path)]
           [beg  ,beg]
           [end  ,end]
           [text ,(intern text)]
           #:or-ignore)))

(define (add-tail-arrow path tail head)
  (query-exec
   (insert #:into tail_arrows #:set
           [path ,(intern path)]
           [head ,head]
           [tail ,tail]
           #:or-ignore)))

(define (add-unused-require path beg end)
  (query-exec
   (insert #:into unused_requires #:set
           [path ,(intern path)]
           [beg  ,beg]
           [end  ,end]
           #:or-ignore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; "commands"

;; Given a file position, see if it is a use of a definition. If so,
;; return a vector describing the definition location, else #f. i.e.
;; This is the basis for "find definition". One wrinkle here is that
;; we may already know about a use of a definition, and which file
;; defines it, but we haven't yet analyzed that defining file. See
;; comments below.
(define (use-pos->def use-path pos #:retry? [retry? #t])
  (match (query-maybe-row
          (select (select str #:from strings #:where (= id def_path))
                  def_beg
                  def_end
                  #:from xrefs
                  #:where (and (= use_path ,(intern use-path))
                               (<= use_beg ,pos) (< ,pos use_end))))
    [(vector def-path (? integer? beg) (? integer? end))
     (vector def-path beg end)]
    [(vector def-path (== db:sql-null) (== db:sql-null))
     (cond [(and retry?
                 (analyze-path def-path))
            (use-pos->def use-path pos #:retry? #f)]
           [else
            (vector def-path 1 1)])]
    [#f #f]))

;; Like use-pos->def, but when the def loc is also a use of another
;; loc --- as with contract-out --- return that other def.
(define (use-pos->def/transitive use-path pos)
  ;; Although we could use a recursive CTE here, we want to use the
  ;; ability of use-pos->def to call analyze on-demand (which we can't
  ;; do inside a SQL query). Anyway, the potential performance benefit
  ;; here is less -- we're "drilling down" linearly to one answer, not
  ;; "fanning out" as with def-pos->uses/transitive.
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
   (match (use-pos->def use-path pos)
     [(and vec (vector def-path def-beg _def-end))
      (if (equal? vec previous-answer)
          vec
          (loop vec def-path def-beg))]
     [#f previous-answer])))

(define (def-pos->uses path pos)
  (query-rows
   (select (select str #:from strings #:where (= strings.id use_path))
           (select str #:from strings #:where (= strings.id def_text))
           (select str #:from strings #:where (= strings.id use_text))
           (select str #:from strings #:where (= strings.id use_stx))
           use_beg
           use_end
           #:from xrefs
           #:where (and (= def_path ,(intern path))
                        (<= def_beg ,pos) (< ,pos def_end))
           #:order-by use_path use_beg)))

;; Like def-pos->uses, but when a use loc is also a def --- as with
;; contract-out --- also return uses of that other def.
(define (def-pos->uses/transitive path pos)
  ;; We can do this using a recursive CTE.
  (query-rows
   (sql
    (with
     #:recursive
     ([(rec def_path def_beg def_end
            use_path use_beg use_end
            def_text use_text use_stx)
       (union
        (select xrefs.def_path xrefs.def_beg xrefs.def_end
                xrefs.use_path xrefs.use_beg xrefs.use_end
                xrefs.def_text xrefs.use_text xrefs.use_stx
                #:from xrefs
                #:where (and (= def_path ,(intern path))
                             (<= def_beg ,pos)
                             (< ,pos def_end)))
        (select xrefs.def_path xrefs.def_beg xrefs.def_end
                xrefs.use_path xrefs.use_beg xrefs.use_end
                xrefs.def_text xrefs.use_text xrefs.use_stx
                #:from (inner-join
                        rec xrefs
                        #:on
                        (and (= rec.use_path xrefs.def_path)
                             (= rec.use_beg  xrefs.def_beg)
                             (= rec.use_end  xrefs.def_end)))))])
     (select (select str #:from strings #:where (= id use_path))
             (select str #:from strings #:where (= strings.id def_text))
             (select str #:from strings #:where (= strings.id use_text))
             (select str #:from strings #:where (= strings.id use_stx))
             use_beg
             use_end
             #:from rec
             #:order-by use_path use_beg)))))

;; Find a definition position given a module path and symbol. Only for
;; module-level (not lexical) definitions.
(define (find-def path subs symbol)
  (query-maybe-row
   (select beg
           end
           #:from defs
           #:where (and (= path  ,(intern path))
                        (= subs  ,(intern subs))
                        (= sym   ,(intern symbol))
                        (<> kind 0)))))

;; This is somewhat like "search docs" but for definitions. Only
;; considers module-level (not lexical) definitions. Only considers
;; the actual definition ID (not the nominal-id or rename aliases) so
;; not necessarily useful in the face of things like racket/contract
;; wrapper functions.
(define (find-all-defs-named symbol)
  (query-rows
   (select (select str #:from strings #:where (= strings.id path))
           (select str #:from strings #:where (= strings.id subs))
           (select str #:from strings #:where (= strings.id sym))
           beg
           end
           #:from defs
           #:where (= sym ,(intern symbol)))))

;;; renaming

(define (use-pos->name path pos)
  ;; This Racket-heavy code seems to work (except for rename-in not
  ;; yet implemented) but just in one direction -- from uses to
  ;; name-introductions.
  ;;
  ;; It would be better to have a `noms` view, which is like `arrows`
  ;; but for uses and introdutions of names. That way, it's a two-way
  ;; relation, and we can query it either way, just like we do for
  ;; uses and definitions.
  ;;
  ;; The catch is that there are quite a few variations -- local,
  ;; imported, imported and renamed, imported from a renaming provide
  ;; -- some of which are their own non-trivial view. Also, there's a
  ;; "logical OR" quality to choosing which of those, and that's a bit
  ;; awkward as either a join or as redundant case statements.
  ;;
  ;; Rather than attempt such a complicated query, it might be better
  ;; to shift more such work toward db insert time. For example,
  ;; things like rename-in or rename-out involve inferring a missing
  ;; arrow, based on other existing arrows. We could add such arrows
  ;; in the first place -- whether to `arrows` or to an additional
  ;; 'name_arrows' table, is TBD. Probably the latter is best, due to
  ;; wrinkles like prefix-in prefixes needing to be treated
  ;; differently.
  (match (query-maybe-row
          (select
           arrows.kind
           (select str #:from strings #:where (= id (case #:of arrows.kind [0 arrows.use_path] [else arrows.nom_path])))
           (case #:of arrows.kind [0 arrows.def_text] [else arrows.nom_id])
           (case #:of arrows.kind [0 arrows.def_text] [else arrows.from_id])
           (case #:of arrows.kind [0 arrows.def_beg] [else rhs.def_beg])
           (case #:of arrows.kind [0 arrows.def_end] [else rhs.def_end])
           #:from (left-join
                    (as (select
                          ;; Hack for prefix-in: Say that kind is
                          ;; lexical not require. We want the arrow
                          ;; pointing into the prefix-in form.
                          (as (case [(= (select str #:from strings #:where (= id use_stx))
                                        (|| (select str #:from strings #:where (= id use_text))
                                            (select str #:from strings #:where (= id nom_id))))
                                     0]
                                [else kind]) kind)
                          *
                          #:from arrows)
                        arrows)
                   (as arrows rhs)
                   #:on (and (<> arrows.kind 0) ;require
                             (= rhs.kind 0)     ;lexical
                             (= arrows.nom_path rhs.use_path)
                             (= arrows.nom_subs rhs.nom_subs)
                             (= arrows.nom_id   rhs.nom_id)))
           #:where (and (= arrows.use_path ,(intern path))
                        (<= arrows.use_beg ,pos) (< ,pos arrows.use_end))
           #:limit 1))
    [(vector kind nom-path nom-id from-id beg end)
     (cond [(and (= kind 0)) ;lexical
            (vector nom-path beg end)]
           [(equal? nom-id from-id) ;non-renaming provide
            (vector nom-path beg end)]
           [else ;renaming provide
            (query-maybe-row
             (select
              (select str #:from strings #:where (= id use_path))
              use_beg
              use_end
              #:from arrows
              #:where (and (= arrows.use_path ,(intern nom-path))
                           (= arrows.def_beg  ,beg)
                           (= arrows.def_end  ,end)
                           (= arrows.from_id  ,from-id)
                           (= arrows.nom_id   ,nom-id)
                           (= arrows.use_text ,nom-id))))])]
    [#f #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyzing more discovered files

;; Analyzing a file will discover more files to analyze. Rather than
;; embark on a depth-first traversal of the universe -- which might
;; consume a very large amount of memory, and, which might delay a
;; command response for which we inititated the original analysis --
;; we put these paths into an async channel to analyze later.

(define analyze-more-files-thread #f)

(define stop-ch (make-channel))
(define todo-ach (make-async-channel))

(define (start-analyze-more-files-thread)
  (define (on-more-files paths)
    (define n (set-count paths))
    (log-pdb-debug
     "analyze-more-files-thread got ~v more files to check: ~v" n paths)
    (set-for-each paths analyze-path)
    (log-pdb-debug
     "analyze-more-files-thread analyzed or skipped ~v files" n)
    (analyze-more-files))
  (define (analyze-more-files)
    (sync (handle-evt stop-ch void)             ;exit thread
          (handle-evt todo-ach on-more-files))) ;recur
  (unless (db:connection? (current-dbc))
    (error 'start-analyze-more-files-thread "no connection; call `open` first"))
  (log-pdb-info "started analyze-more-files-thread")
  (set! analyze-more-files-thread
        (thread analyze-more-files)))

(define (stop-analyze-more-files-thread)
  (when analyze-more-files-thread
    (define thd analyze-more-files-thread)
    (set! analyze-more-files-thread #f)
    (log-pdb-info "asking analyze-more-files-thread to stop")
    (channel-put stop-ch 'stop)
    (thread-wait thd)
    (log-pdb-info "analyze-more-files-thread exited")))

(define (queue-more-files-to-analyze paths)
  (when analyze-more-files-thread
    (async-channel-put todo-ach paths)))
