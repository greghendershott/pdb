;; Copyright (C) 2021 by Greg Hendershott

#lang at-exp racket/base

(require drracket/check-syntax
         openssl/sha1
         racket/async-channel
         racket/class
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/sequence
         sql
         syntax/modread
         syntax/parse/define)

;;; logger/timing

(define-logger definitions)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-definitions-debug "~a cpu | ~a real | ~a gc <= ~a"
                         (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check-syntax

;; Note: drracket/check-syntax reports things as zero-based [from upto)
;; but we handle them as one-based [from upto).

(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (define more-files null)

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-definition-target _ beg end symbol rev-mods)
      (define submods (reverse rev-mods))
      (add-def src (add1 beg) (add1 end) submods symbol))

    (define/override (syncheck:add-jump-to-definition _ beg end reported-id path submods)
      (when (file-exists? path)
        (define source-id (string->symbol (substring code-str beg end)))
        (add-use src (add1 beg) (add1 end) path submods reported-id source-id)
        (set! more-files (cons path more-files))))

    ;; Handling this lets us also record uses within this source file
    ;; (which aren't reported by syncheck:add-jump-to-definition) of
    ;; definitions reported by syncheck:add-definition-target.
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      _def-src def-beg def-end _def-px _def-py
                      _use-src use-beg use-end _use-px _use-py
                      _actual? _level require-arrow? _name-dup?)
      (unless require-arrow? ;unless covered by add-jump-to-definition
        ;; This doesn't state the submods for the definition. Try to
        ;; look that up for a definition target we already recorded at
        ;; this location. If that lookup fails, then ignore this
        ;; (which is probably fine because this is probably an arrow
        ;; pointing to a lexical variable.)
        (define sym (string->symbol (substring code-str def-beg def-end)))
        (define submods (get-definition-submods-str sym src (add1 def-beg) (add1 def-end)))
        (when submods
          (add-use src (add1 use-beg) (add1 use-end) src submods sym sym))))

    (define/override (syncheck:add-require-open-menu _ _beg _end file)
      (set! more-files (cons file more-files)))

    (define/public (notify-more-files-to-analyze)
      (add-files-to-analyze (remove-duplicates more-files)))

    (super-new)))

(define (string->syntax path code-str [k values])
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (define in (open-input-string code-str path))
         (port-count-lines! in)
         (match (read-syntax path in)
           [(? eof-object?) #'""]
           [(? syntax? stx) stx]))))))

(define (analyze-path path)
  (analyze-code path (file->string path #:mode 'text)))

(define (analyze-code path code-str)
  (cond
    [(update-digest path (sha1 (open-input-string code-str)))
     (with-time/log (~a "analyze " (~v (str path)))
       (delete-uses-and-defs-involving path)
       (string->syntax
        path
        code-str
        (λ (stx)
          (parameterize ([current-namespace (make-base-namespace)])
            (define exp-stx (expand stx))
            (parameterize ([current-annotations (new annotations-collector%
                                                     [src path]
                                                     [code-str code-str])])
              (define-values (expanded-expression expansion-completed)
                (make-traversal (current-namespace)
                                (current-load-relative-directory)))
              (expanded-expression exp-stx)
              (expansion-completed)
              (send (current-annotations) notify-more-files-to-analyze))
            (analyze-contract/provide-transformers path exp-stx)))))
     #t]
    [else
     ;;(log-definitions-debug "  unchanged: ~v" (str path))
     #f])  )

(define (analyze-contract/provide-transformers path stx)
  (define (maybe-add-def-alias path submods old new)
    (match (query-rows
            (select beg end
                    #:from defs
                    #:where (and (= path    ,(intern path))
                                 (= submods ,(intern submods))
                                 (= sym     ,(intern old)))))
      [(list (vector beg end))
       (log-definitions-debug "~v" `(maybe-add-def-alias ,path ,beg ,end ,submods ,old ,new))
       (add-def path beg end submods new)]
      [(list) (void)]))

  (define (symbolic-compare? x y)
    (eq? (syntax-e x) (syntax-e y)))

  (define (handle-file-module stx)
    (syntax-case* stx (module) symbolic-compare?
      [(module _id _lang (_mb . es))
       (handle-module-level '() #'es)]))

  (define (handle-module-level submods es)
    (for ([e (in-syntax es)])
      (syntax-case* e (#%app define-syntaxes make-provide/contract-transformer
                        quote-syntax) symbolic-compare?
        [(define-syntaxes
           (_id)
           (#%app
            make-provide/contract-transformer
            (quote-syntax wrapper-id)
            (quote-syntax _contract-id)
            (quote-syntax original-id)
            _bool1
            _bool2
            (quote-syntax _)
            (quote-syntax _)))
         (maybe-add-def-alias path submods
                              (syntax-e #'original-id)
                              (string->symbol
                               (~a (syntax-e #'wrapper-id) ".1")))]
        [(module id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [(module* id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [_ (void)])))

  (handle-file-module stx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; db

(define current-dbc (make-parameter #f))
(require syntax/parse/define
         (for-syntax racket/base
                     racket/syntax)
         (prefix-in db: db))
(define-syntax-parser define-db
  [(_ id:id)
   #:with real-id (format-id #'id "db:~a" #'id)
   #'(define (id . args) (apply real-id (current-dbc) args))])
(define-db query)
(define-db query-exec)
(define-db query-rows)
(define-db query-maybe-row)
(define-db query-value)
(define-db query-maybe-value)
(define-db query-list)

(define-simple-macro (with-transaction e:expr ...+)
  (db:call-with-transaction (current-dbc) (λ () e ...)))

(define (create-tables)
  ;; The `strings` table ~= interned symbols. There are many long,
  ;; repeated strings, such as for paths, submods, identifiers --
  ;; these can be replaced by integer foreign keys. Saves much space.
  ;; Also speeds queries that test for equality. OTOH it complicates
  ;; some queries by requiring a table join.
  ;;
  ;; (Although this might seem similar also to a "snowflake schema" in
  ;; a data warehouse, that would be the case only if we had a table
  ;; for each kind of thing a.k.a. "dimension". Instead this is really
  ;; just interning. Even so, it does makes talbles like `uses` and
  ;; `defs` look a lot like "fact" tables whose values consist solely
  ;; of integers, many of which are foreign keys into `strings`.)
  (query-exec
   (create-table
    #:if-not-exists strings
    #:columns
    [id          integer #:not-null]
    [str         string  #:not-null]
    #:constraints
    (primary-key id)
    (unique      str)))
  ;; This table of paths and sha-1 digest is how we record whether to
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
  ;; A table of definitions discovered in files. Each definition is
  ;; uniquely identified by -- i.e. the primary key is -- the triple
  ;; (path submods sym). The only other information we record about a
  ;; def is its [beg end) location within the file.
  (query-exec
   (create-table
    #:if-not-exists defs
    #:columns
    [path        integer #:not-null]
    [submods     integer #:not-null]
    [sym         integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key path submods sym)
    (foreign-key path #:references (strings id))
    (foreign-key submods #:references (strings id))
    (foreign-key sym #:references (strings id))))
  ;; A table of uses (of definitions) discovered in files. A use
  ;; refers to a definition via a (path submods symbol) triple --
  ;; there is no strict foreign key pointing to the `defs` table --
  ;; because that triple is all that check-syntax reports to us, and
  ;; futhermore, we will often discover uses before we've analyzed the
  ;; file in which they are defined. In other words, this db does not
  ;; wait to supply any answers until it has traversed the universe in
  ;; an effort to supply all answers. Instead it supplies answers
  ;; known so far, and it can be updated to supply more answers, on
  ;; demand.
  (query-exec
   (create-table
    #:if-not-exists uses
    #:columns
    ;; These columns uniquely identify a use location; in fact we
    ;; explicitly specify them as the primary key for this table:
    [usepath     integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    ;; The remaining columns form a kind of weak/lazy key into the
    ;; `defs` table. We record both the identifier symbol reported by
    ;; check-syntax and the one apparent in the source code -- because
    ;; in various cases involving contracts and renaming, either one
    ;; could turn out to be the identifier used at the definition
    ;; site.
    [defpath     integer #:not-null]
    [submods     integer #:not-null]
    [reportedsym integer #:not-null]
    [sourcesym   integer]
    #:constraints
    (primary-key usepath beg end)
    (foreign-key usepath #:references (strings id))
    (foreign-key defpath #:references (strings id))
    (foreign-key reportedsym #:references (strings id))
    (foreign-key sourcesym #:references (strings id))
    (foreign-key submods #:references (strings id))))

  ;; Optional convenience view: A left outer join of uses on defs.
  ;; Columns that are foreign keys to the `strings` table are included
  ;; (for use in queries) as well as columns looking up the strings
  ;; (for output). As a left join, columns only available from the
  ;; defs table may of course be sql-null. This means we have a use
  ;; and we've been told the defining file path, but we haven't yet
  ;; analyzed that file (or we did analyze it there's some edge case
  ;; where we can't find the definition). [Note: I'm not sure I'll
  ;; even use this view in "real code". But it's useful for
  ;; documentation value, and also to "dump" the table in a human
  ;; readable "stringy" form for inspecting.]
  (query-exec
   (create-view
    UsesToDefs
    (select
     (as uses.usepath usepath_id)
     (as (select str #:from strings #:where (= uses.usepath strings.id)) usepath_str)
     (as uses.beg use_beg)
     (as uses.end use_end)
     (as uses.submods submods_id)
     (as (select str #:from strings #:where (= uses.submods strings.id)) submods_str)
     (as reportedsym reportedsym_id)
     (as (select str #:from strings #:where (= reportedsym strings.id)) reportedsym_str)
     (as sourcesym sourcesym_id)
     (as (select str #:from strings #:where (= sourcesym strings.id)) sourcesym_str)
     (as uses.defpath defpath_id)
     (as (select str #:from strings #:where (= uses.defpath strings.id)) defpath_str)
     (as defs.beg def_beg)
     (as defs.end def_end)
     #:from
     (left-join uses defs
                #:on (and (= uses.defpath defs.path)
                          (= uses.submods defs.submods)
                          (= uses.reportedsym defs.sym)))))))

(define (create-database path)
  (unless (file-exists? path)
    (parameterize ([current-dbc (db:sqlite3-connect #:database  path
                                                    #:mode      'create
                                                    #:use-place #f)])
      (create-tables)
      (db:disconnect (current-dbc)))))

(define (connect [db 'memory])
  (current-dbc (db:sqlite3-connect #:database  db
                                   #:mode      'read/write
                                   #:use-place (not (eq? db 'memory))))
  (query-exec "PRAGMA foreign_keys = on"))

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
;; or when force? is true, in which case all known files are analyzed.
;; Either way, the usual behavior of analyze-path applies: When
;; analyzing a file discovers dependencies on other files, it
;; schedules those to be analyzed (when new/changed), too.
(define (analyze-all-known-paths #:force? [force? #f])
  (for ([path (in-list
               (query-list
                (select str #:from (inner-join
                                    digests strings
                                    #:on (= digests.path strings.id)))))])
    (when force? (forget-digest path))
    (analyze-path path)))

(define (delete-uses-and-defs-involving path)
  (define pathid (intern path))
  (query-exec
   (delete #:from uses #:where (= usepath ,pathid)))
  (query-exec
   (delete #:from defs #:where (= path ,pathid))))

(define (add-def path beg end submods symbol)
  (query-exec
   (insert #:into defs #:set
           [path    ,(intern path)]
           [submods ,(intern submods)]
           [sym     ,(intern symbol)]
           [beg     ,beg]
           [end     ,end]
           ;; FIXME: check-syntax will report identical
           ;; (path symbol submods) for a file-module-level
           ;; define and one inside a module+ form. :( I'd
           ;; expect submods to include the module+ name
           ;; but it does not. So for now simply ignore
           ;; any such shadowing definitions.
           #:or-ignore)))

(define (get-definition-submods-str sym path beg end)
  (query-maybe-value
   (select (select str #:from strings #:where (= id submods))
           #:from defs
           #:where (and (= sym ,(intern sym))
                        (= path ,(intern path))
                        (= beg ,beg)
                        (= end ,end)))))

(define (add-use use-path use-beg use-end def-path def-submods reported-sym source-sym)
  (query-exec
   (insert #:into uses #:set
           [usepath     ,(intern use-path)]
           [beg         ,use-beg]
           [end         ,use-end]
           [defpath     ,(intern def-path)]
           [submods     ,(intern def-submods)]
           [reportedsym ,(intern reported-sym)]
           [sourcesym   ,(intern source-sym)]
           ;; For things like `struct`, check-syntax might duplicate
           ;; syncheck:add-jump-to-definition.
           #:or-ignore)))

;; Given module path and symbol, return all known uses. i.e. This is
;; is the basis for a "find references" command, as well as a
;; multi-file "rename definition and references" command.
(define (get-uses path submods symbol)
  (define symid (intern symbol))
  (query-rows
   (select (select str #:from strings #:where (= strings.id usepath))
           beg
           end
           #:from uses
           #:where (and (= defpath ,(intern path))
                        (= submods ,(intern submods))
                        (= reportedsym ,symid)))))


;; Given use of a def, return all uses of the def. i.e. "Find all
;; similar references".
(define (use-pos->def-and-use-locs use-path pos)
  (match (use-pos->def-loc use-path pos)
    [(and def-loc
          (vector def-path submods (? string? sym) _beg _end))
     (values def-loc
             (get-uses def-path submods sym))]
    [_ #f]))

;; Given a file position, see if it is a use of a definition. If so,
;; return a vector describing the definition location, else #f. i.e.
;; This is the basis for "find definition". One wrinkle here is that
;; we may already know about a use of a definition, and which file
;; check-syntax and identifier-binding believe defines it, but we
;; haven't yet analyzed that defining file. See comments below.
(define (use-pos->def-loc use-path pos)
  (let loop ([first-attempt? #t])
    (match
      (query-maybe-row
       (select (select str #:from strings #:where (= strings.id uses.defpath))
               (select str #:from strings #:where (= strings.id uses.submods))
               defs.beg
               defs.end
               #:from
               (left-join
                (as (select defpath submods reportedsym sourcesym
                            #:from uses
                            #:where (and (= usepath ,(intern use-path))
                                         (<= beg ,pos) (< ,pos end)))
                    uses)
                defs
                #:on (and (= uses.defpath defs.path)
                          (= uses.submods defs.submods)
                          (= uses.reportedsym defs.sym)))))
      ;; If we know it is a reference to a definition in a file, but
      ;; not the location within the file, it could mean we haven't
      ;; analyzed that other file yet. If so, analyze it now, then try
      ;; again. things:
      [(vector def-path subs (? db:sql-null?) (? db:sql-null?))
       (cond [(and first-attempt?
                   (analyze-path def-path))
              ;; Although this will automatically happen eventually we
              ;; need it done now:
              (resolve-transitive-uses->defs)
              (loop #f)]
             [else (vector def-path subs #f 1 1)])]
      [(? vector? vec) vec]
      [_ #f])))

;; Check all uses. If a reported definition file doesn't actually
;; contain a definition, check whether the symbol matches a /use/ in
;; the ostensible defining file, pointing to the definition in some
;; other file. This happens fairly often with contracts/renames.
;; Check-syntax reports that a use of foo in a.rkt is defined in b.rkt
;; -- but it's only just /used/ in b.rkt and it's /defined/in c.rkt.
;; Here we find such situations and update the use from a.rkt to point
;; directly to c.rkt.
;;
;; EXAMPLE: A good example is the use of `make-traversal` in this
;; source file. Supposedly it's defined in drracket/check-syntax.rkt,
;; which we dutifully recorded in the `uses` db table. But it's only
;; used there; instead see drracket/syncheck/private/traversals.rkt.
;;
;; This function is intended to be called after the end of one or more
;; uses of `analyze-path` -- after it has added/changed any data, so
;; we can do this fixup. It is wasteful to call it after every single
;; analyze.
(define (resolve-transitive-uses->defs)
  (with-time/log 'resolve-transitive-uses->defs
    (define count (do-resolve-transitive-uses->defs))
    (log-definitions-debug "Resolved ~v uses" count)
    count))
(define (do-resolve-transitive-uses->defs)
  ;; TODO: Rewrite as a single "UPDATE FROM" query?
  (define q
    (select
     (as missing.usepath use_path)
     (as missing.beg use_beg)
     (as missing.end use_end)
     (as transitive.path new_def_path)
     (as transitive.submods new_sub_mods)
     #:from
     (inner-join
      ;; Uses anywhere that think their definition is in def-path, but
      ;; submods / beg / end are null.
      (as (select uses.usepath
                  uses.beg
                  uses.end
                  uses.defpath
                  uses.submods
                  reportedsym
                  sourcesym
                  #:from
                  (left-join
                   uses defs
                   #:on (and (= uses.defpath defs.path)
                             (= uses.submods defs.submods)
                             (= uses.reportedsym defs.sym)))
                  #:where (or (is-null defs.submods)
                              (is-null defs.beg)
                              (is-null defs.end)))
          missing)
      ;; Uses in def-path that have a definition with beg and
      ;; end that is /not/ sql-null.
      (as (select #:distinct
                  uses.usepath
                  defs.path
                  defs.submods
                  reportedsym
                  sourcesym
                  #:from (inner-join
                          uses defs
                          #:on (and (= uses.defpath defs.path)
                                    (= uses.submods defs.submods)
                                    (= uses.reportedsym defs.sym))))
          transitive)
      #:on (and (= missing.defpath transitive.usepath)
                (or (= missing.submods transitive.submods)
                    ;; This handles e.g. `from-m` in
                    ;; example/define.rkt.
                    (= transitive.usepath transitive.path))
                (= missing.reportedsym transitive.reportedsym)))))
  (for/sum ([(use-path use-beg use-end new-def-path new-sub-mods)
             (db:in-query (current-dbc) q)])
    (log-definitions-debug
     "Update ~v ~v ~v to ~v ~v"
     (query-value (select str #:from strings #:where (= id ,use-path)))
     use-beg
     use-end
     (query-value (select str #:from strings #:where (= id ,new-def-path)))
     (query-value (select str #:from strings #:where (= id ,new-sub-mods))))
    (query-exec (update uses
                        #:set
                        [defpath ,new-def-path]
                        [submods ,new-sub-mods]
                        #:where (and (= usepath ,use-path)
                                     (= beg ,use-beg)
                                     (= end ,use-end))))
    1))

(define (str v)
  (cond [(path? v)  (path->string v)]
        [(number? v) v]
        [else        (~a v)]))

(define (un-str s)
  (read (open-input-string s)))

;;; Analyzing more discovered files

;; Analyzing a file will discover more files to analyze. Rather than
;; embark on a depth-first traversal of the universe -- which might
;; consume a very large amount of memory, and, which might delay a
;; command response for which we inititated the original analysis --
;; we put these paths into an async channel to analyze later.
;;
;; In addition we want to call resolve-transitive-uses->defs to do
;; fixups, but, it makes sense to wait to do that until new file
;; analyses have "settled down" for awhile.

(define ach-todo (make-async-channel))

(define (add-files-to-analyze paths)
  (async-channel-put ach-todo paths))

(define (start-analyze-all-todo-thread)
  (define (analyze-all-todo-thread)
    (log-definitions-info "started analyze-all-todo-thread")
    (let loop ([resolve-timer-evt never-evt])
      (sync (handle-evt ach-todo
                        (λ (paths)
                          (map analyze-path paths)
                          (loop (alarm-evt (+ (current-inexact-milliseconds)
                                              5000)))))
            (handle-evt resolve-timer-evt
                        (λ (_)
                          (resolve-transitive-uses->defs)
                          (loop never-evt))))))
  (thread analyze-all-todo-thread))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples

(module+ in-memory-example
  (connect 'memory)
  (create-tables)
  (start-analyze-all-todo-thread)
  (add-def "/a/b/c.rkt" 42 46 '(a b c) 'foo)
  (add-use "/d/e/f.rkt" 11 14 "/a/b/c.rkt" '(a b c) 'foo 'foo)
  (add-use "/d/e/f.rkt" 90 93 "/a/b/c.rkt" '(a b c) 'foo 'foo)
  (query-rows "select * from digests")
  (query-rows "select * from defs")
  (query-rows "select * from uses")
  (get-uses "/a/b/c.rkt" '(a b c) 'foo))

(module+ on-disk-example
  (require racket/runtime-path
           rackunit)
  (define-runtime-path db-path "locs.sqlite")
  (create-database db-path)
  (connect db-path)
  (start-analyze-all-todo-thread)
  (define-runtime-path define.rkt "example/define.rkt")
  (define-runtime-path require.rkt "example/require.rkt")
  (forget-digest (build-path define.rkt))
  (forget-digest (build-path require.rkt))
  (analyze-path (build-path require.rkt))
  ;; Test that various uses in example/require.rkt point to the
  ;; correct definition location in example/define.rkt.
  (check-equal? (use-pos->def-loc require.rkt 42)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 88 93))
  (check-equal? (use-pos->def-loc require.rkt 48)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 88 93))
  (check-equal? (use-pos->def-loc require.rkt 56)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 165 176))
  (check-equal? (use-pos->def-loc require.rkt 68)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 246 257))
  (check-equal? (use-pos->def-loc require.rkt 80)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 322 325))
  (check-equal? (use-pos->def-loc require.rkt 99)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 515 529))
  (check-equal? (use-pos->def-loc require.rkt 114)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 684 703))
  (check-equal? (use-pos->def-loc require.rkt 134)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "(sub)" 958 961))
  (check-equal? (use-pos->def-loc require.rkt 138)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "(sub)" 958 961))
  (check-equal? (use-pos->def-loc require.rkt 150)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 1179 1182))
  (check-equal? (use-pos->def-loc require.rkt 154)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 1225 1233))
  (check-equal? (use-pos->def-loc require.rkt 163)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "()" 1265 1276))
  (check-equal? (use-pos->def-loc require.rkt 175)
                '#("/home/greg/src/racket/pdb/example/define.rkt" "(m)" 1353 1359))
  ;; Re-analyze this file (and watch the `definitions` logger topic)
  (define-runtime-path main.rkt "main.rkt")
  (forget-digest (build-path main.rkt))
  (analyze-path (build-path main.rkt)))

;; 1. One idea here would be to replace the Racket Mode back end
;; check-syntax code with this: The front end would request an
;; analysis, the back end would notify when it's ready, and the front
;; end would issue commands to discover info for various ranges of the
;; buffer. Unclear if it would still propertize text ranges with
;; some/all of that returned info, or instead, if it could just query
;; the back end as needed. Although this could probably work, it would
;; be slower. Our analysis here takes ~ 1.5X to 2X the time, due to db
;; writing/reading overhead. [OTOH it might be a way to contribute to
;; fixing the "streaming for very large source files" issue samth
;; reported.]
;;
;; 2. So another idea is that the RMBE would still do and return its
;; analysis, status quo. It's just that, /in addition/, it would
;; submit these results to the db -- presumably using another thread
;; that doesn't delay the command response to the front end. That way,
;; the front end check-sytnax would not change in method or timing.
;; However, we could support RM commands (and external tools) that use
;; inter-file references. Also, we could get rid of the RMBE's syntax
;; caching, I think: Things like find-definition need not walk
;; fully-expanded syntax to look for the definition site -- we already
;; did that and saved the result for all possible definitions,
;; beforehand. That is just a db query. (I'm not sure about
;; find-signature: maybe we could add a pass to walk non-expanded
;; syntax, and store that extra info in a new column in the `defs`
;; table, or, store it in a new `sigs` table.)
