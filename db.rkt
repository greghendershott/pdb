;; Copyright (C) 2021 by Greg Hendershott

#lang at-exp racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (prefix-in db: db)
         openssl/sha1
         syntax/parse/define
         racket/async-channel
         racket/file
         racket/list
         racket/match
         sql
         "common.rkt")

(provide open
         create-database
         create-tables
         analyze-path

         forget-digest
         add-def
         add-use
         queue-more-files-to-analyze
         str

         use-pos->def
         use-pos->def/transitive
         get-uses
         get-uses/transitive)

(define (open what analyze-code-proc)
  (current-analyze-code analyze-code-proc)
  (connect what)
  (start-analyze-more-files-thread))

(define current-analyze-code (make-parameter void))
(define (analyze-path path)
  (define code-str (file->string path #:mode 'text))
  (define digest (sha1 (open-input-string code-str)))
  (and (update-digest path digest)
       (with-time/log (format "analyze ~v" (str path))
         (delete-uses-and-defs-involving path)
         ((current-analyze-code) path code-str)
         #t)))

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
  ;; `uses` and `defs` look a lot like data warehouse "fact" tables,
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
  ;; A table of definitions discovered in files.
  (query-exec
   (create-table
    #:if-not-exists defs
    #:columns
    ;; Each definition is uniquely identified by -- i.e. the primary
    ;; key is -- the triple (path submods sym).
    [path        integer #:not-null]
    [submods     integer #:not-null]
    [sym         integer #:not-null]
    ;; Otherwise we just record the [beg end) location within the
    ;; file.
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key path submods sym)
    (foreign-key path #:references (strings id))
    (foreign-key submods #:references (strings id))
    (foreign-key sym #:references (strings id))))
  ;; A table of uses (of definitions) discovered in files.
  (query-exec
   (create-table
    #:if-not-exists uses
    #:columns
    ;; These columns uniquely identify a use location; in fact we
    ;; explicitly specify them as the primary key for this table:
    [usepath     integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    ;; A use relates to a definition via a (path submods symbol)
    ;; triple. But this is a "lazy" reference; there is no strict
    ;; foreign key constraint. Why: We often discover uses before
    ;; we've analyzed the file in which they are defined. So although
    ;; we believe identifier-binding that a definition using that
    ;; identifier symbol is probably in that file+submods, we don't
    ;; yet know its loc and we haven't yet created a row in the `defs`
    ;; table for it. In other words, this db does not try to find ALL
    ;; answers (by traversing the entire universe) before being able
    ;; to give ANY answers. Instead: It supplies answers known so far,
    ;; and it can be updated to supply more answers, on demand.
    [defpath     integer #:not-null]
    [submods     integer #:not-null]
    [sym         integer #:not-null] ;source-id not nominal-id
    #:constraints
    (primary-key usepath beg end)
    (foreign-key usepath #:references (strings id))
    (foreign-key defpath #:references (strings id))
    (foreign-key sym #:references (strings id))
    (foreign-key submods #:references (strings id))))

  ;;; Optional convenience views
  ;;;
  ;;; These aren't necessarily efficient; not intended for "real" use.
  ;;; However they have some documentation value, and, they can be
  ;;; handy when debugging, to explore seeing "de-interned", stringy
  ;;; values.
  (query-exec
   (create-view
    DefsView
    (select
     (as (select str #:from strings #:where (= strings.id path))    path)
     (as (select str #:from strings #:where (= strings.id submods)) submods)
     (as (select str #:from strings #:where (= strings.id sym))     sym)
     beg
     end
     #:from defs)))
  ;; `uses` but "un-interns" the string IDs back to strings
  (query-exec
   (create-view
    UsesView
    (select
     (as (select str #:from strings #:where (= strings.id usepath)) usepath)
     beg
     end
     (as (select str #:from strings #:where (= strings.id defpath)) defpath)
     (as (select str #:from strings #:where (= strings.id submods)) submods)
     (as (select str #:from strings #:where (= strings.id sym))     sym)
     #:from uses)))
  ;; A left outer join of uses on defs. Columns that are foreign keys
  ;; to the `strings` table are included (for use in queries) as well
  ;; as columns looking up the strings (for output). As a left join,
  ;; columns only available from the defs table may of course be
  ;; sql-null. This means we have a use and we've been told the
  ;; defining file path, but we haven't yet analyzed that file (or we
  ;; did analyze it there's some edge case where we can't find the
  ;; definition).
  (query-exec
   (create-view
    UsesToDefsView
    (select
     (as uses.usepath usepath_id)
     (as (select str #:from strings #:where (= strings.id uses.usepath)) usepath_str)
     (as uses.beg use_beg)
     (as uses.end use_end)
     (as uses.submods submods_id)
     (as (select str #:from strings #:where (= strings.id uses.submods)) submods_str)
     (as uses.sym sym_id)
     (as (select str #:from strings #:where (= strings.id uses.sym)) sym_str)
     (as uses.defpath defpath_id)
     (as (select str #:from strings #:where (= uses.defpathλ strings.id)) defpath_str)
     (as defs.beg def_beg)
     (as defs.end def_end)
     #:from
     (left-join uses defs
                #:on (and (= uses.defpath defs.path)
                          (= uses.submods defs.submods)
                          (= uses.sym     defs.sym)))))))

(define (create-database path)
  (unless (file-exists? path)
    (log-definitions-warning "~v does not exist; creating it and tables" path)
    (parameterize ([current-dbc (db:sqlite3-connect #:database  path
                                                    #:mode      'create
                                                    #:use-place #f)])
      (create-tables)
      (db:disconnect (current-dbc)))))

(define (connect db)
  (current-dbc (db:sqlite3-connect #:database  db
                                   #:mode      'read/write
                                   #:use-place (not (eq? db 'memory))))
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
  (query-exec "pragma synchronous = off"))

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
;; analyzing a file discovers dependencies on other files, it queues
;; those to be analyzed, too.
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
  (query-exec (delete #:from uses #:where (= usepath ,pathid)))
  (query-exec (delete #:from defs #:where (= path ,pathid))))

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

(define (add-use use-path use-beg use-end def-path def-submods sym)
  (query-exec
   (insert #:into uses #:set
           [usepath ,(intern use-path)]
           [beg     ,use-beg]
           [end     ,use-end]
           [defpath ,(intern def-path)]
           [submods ,(intern def-submods)]
           [sym     ,(intern sym)]
           ;; For things like `struct`, check-syntax might duplicate
           ;; syncheck:add-jump-to-definition.
           #:or-ignore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "commands"

;; Given a file position, see if it is a definition.
(define (def-pos->def path pos)
  (query-maybe-row
   (select (select str #:from strings #:where (= strings.id path))
           (select str #:from strings #:where (= strings.id submods))
           (select str #:from strings #:where (= strings.id sym))
           beg
           end
           #:from defs
           #:where (and (= path ,(intern path))
                        (<= beg ,pos) (< ,pos end)))))

;; Given a file position, see if it is a use of a definition. If so,
;; return a vector describing the definition location, else #f. i.e.
;; This is the basis for "find definition". One wrinkle here is that
;; we may already know about a use of a definition, and which file
;; defines it, but we haven't yet analyzed that defining file. See
;; comments below.
(define (use-pos->def use-path pos #:retry? [retry? #t])
  (match
      (query-maybe-row
       (select (select str #:from strings #:where (= strings.id uses.defpath))
               (select str #:from strings #:where (= strings.id uses.submods))
               (select str #:from strings #:where (= strings.id uses.sym))
               defs.beg
               defs.end
               #:from
               (left-join
                (as (select defpath submods sym
                            #:from uses
                            #:where (and (= usepath ,(intern use-path))
                                         (<= beg ,pos) (< ,pos end)))
                    uses)
                defs
                #:on (and (= uses.defpath defs.path)
                          (= uses.submods defs.submods)
                          (= uses.sym defs.sym)))))
    ;; If we know it is a reference to a definition in a file, but not
    ;; the location within the file, it could mean we haven't analyzed
    ;; that other file yet. If so, analyze now and retry.
    [(vector def-path subs _sym (? db:sql-null?) (? db:sql-null?))
     (cond [(and retry?
                 (analyze-path def-path))
            (use-pos->def use-path pos #:retry? #f)]
           [else
            (vector def-path subs #f 1 1)])]
    [(? vector? vec) vec]
    [_ #f]))

;; Like use-pos->def, but when the def loc is also a use of another
;; loc --- as with contract-out --- return that other def.
(define (use-pos->def/transitive use-path pos)
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
   (match (use-pos->def use-path pos)
     [(and vec (vector def-path _subs _sym (? integer? beg) _end))
      (loop vec def-path beg)]
     [(? vector? vec) vec]
     [#f previous-answer])))

(define (get-def path submods symbol)
  (query-maybe-row
   (select beg
           end
           #:from defs
           #:where (and (= path    ,(intern path))
                        (= submods ,(intern submods))
                        (= sym     ,(intern symbol))))))

;; Given module path and symbol, return all known uses. This would be
;; suitable for a simple "find references" command. See also
;; get-uses/transitive.
(define (get-uses path submods symbol)
  (query-rows
   (select (select str #:from strings #:where (= strings.id usepath))
           (select str #:from strings #:where (= strings.id sym))
           beg
           end
           #:from uses
           #:where (and (= defpath ,(intern path))
                        (= submods ,(intern submods))
                        (= sym     ,(intern symbol))))))

;; Like get-uses, but when a use loc is also a def --- as with
;; contract-out --- also return uses of that other def.
;;
;; Unlike plain get-uses, this would be suitable as the basis is the
;; basis for a thorough a multi-file "rename definition and
;; references" command. That is, it would cover the "entire chain"
;; of things that need to be renamed.
(define (get-uses/transitive path submods symbol)
  (flatten ;; TODO: Optimize
   (for/list ([use (in-list (get-uses path submods symbol))])
     (match-define (vector path _sym pos _end) use)
     (cons use
           (match (def-pos->def path pos)
             [(vector path submods sym _beg _end)
              (get-uses/transitive path submods sym)]
             [#f null])))))

;; This is somewhat like "search docs" but for defs.
(define (find-defs-named symbol)
  (query-rows
   (select (select str #:from strings #:where (= strings.id path))
           (select str #:from strings #:where (= strings.id submods))
           (select str #:from strings #:where (= strings.id sym))
           beg
           end
           #:from defs
           #:where (= sym ,(intern symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyzing more discovered files

;; Analyzing a file will discover more files to analyze. Rather than
;; embark on a depth-first traversal of the universe -- which might
;; consume a very large amount of memory, and, which might delay a
;; command response for which we inititated the original analysis --
;; we put these paths into an async channel to analyze later.

(define ach-todo (make-async-channel))

(define (queue-more-files-to-analyze paths)
  (async-channel-put ach-todo paths))

(define (start-analyze-more-files-thread)
  (define (analyze-more-files-thread)
    (log-definitions-info "started analyze-more-files-thread")
    (for ([paths (in-producer async-channel-get 'n/a ach-todo)])
      (define n (length paths))
      (log-definitions-debug "analyze-more-files-thread got ~v more files to check" n)
      (map analyze-path paths)
      (log-definitions-debug "analyze-more-files-thread analyzed or skipped ~v files" n)))
  (void (thread analyze-more-files-thread)))


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
