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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; analyze

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
  (and
   (update-digest path (sha1 (open-input-string code-str)))
   (with-time/log (~a "analyze " (~v (str path)))
     (delete-uses-and-defs-involving path)
     (string->syntax
      path
      code-str
      (λ (stx)
        (parameterize ([current-namespace (make-base-namespace)])
          (define exp-stx (expand stx))
          (analyze-using-check-syntax path exp-stx code-str)
          (analyze-provide/contract-transformers path exp-stx))))
     #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; analyze: using check-syntax

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

    (define/override (syncheck:add-jump-to-definition _ beg end sym path submods)
      (when (file-exists? path)
        (add-use src (add1 beg) (add1 end) path submods sym)
        (add-file-to-analyze path)))

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
          (add-use src (add1 use-beg) (add1 use-end) src submods sym))))

    (define/override (syncheck:add-require-open-menu _ _beg _end file)
      (add-file-to-analyze file))

    (define (add-file-to-analyze file)
      (set! more-files (cons file more-files)))

    (define/public (get-more-files-to-analyze)
      (remove-duplicates more-files))

    (super-new)))

(define (analyze-using-check-syntax path exp-stx code-str)
  (parameterize ([current-annotations (new annotations-collector%
                                           [src path]
                                           [code-str code-str])])
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace)
                      (current-load-relative-directory)))
    (expanded-expression exp-stx)
    (expansion-completed)
    (queue-more-files-to-analyze (send (current-annotations)
                                       get-more-files-to-analyze))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The purpose of this is to handle the "weird" situation with
;; contract-out. Given some function "foo", it will define a wrapper
;; function named "provide/contract-id-foo.1", and, export that
;; renamed as "foo". As a result, "provide/contract-id-foo.1" is the
;; source identifier reported by identifier-binding (and therefore
;; syncheck:add-jump-to-definition). The fact that it's a wrapper
;; function is immaterial to identifier-binding; the exported thing is
;; the wrapper, and the defined name of the wrapper is
;; "provide/contract-id-foo.1" --- not "foo".
;;
;; Although drracket/check-syntax does not call
;; syncheck:add-definition-target for the wrapper, it does call
;; syncheck:add-arrow and/or syncheck:jump-to-definition for the /use/
;; of the original in the contract-out form that defines the wrapper.
;; We record those uses as usual, elsewhere. What we add here is that
;; act as if we also got an add-definition-target for the wrapper
;; definition in the contract-out form.
;;
;; Why do we record the wrapper definition? So that uses have
;; something to point to.
;;
;; There is a question of identity or equivalence. Racket Mode tries
;; to jump past the wrapper contract and on to the definition of the
;; original thing. That is, take the user all the way, transitively.
;; For convenience it pretends that the wrapper and original are "the
;; same".
;;
;; After working with this db for awhile, I believe that the db should
;; record the reality: The wrapper and the original functions are two
;; separate definitions. Recording them separately helps because they
;; (a) have different names, (b) might be defined in different source
;; files, and (c) might even be defined in different submodules within
;; one source file. There are various permutations that are tricky or
;; even impossible to handle, when joining uses and defs by a (path
;; submods id) triple.
;;
;; In practice this means that if some user of the db wants the
;; "transitive" interpretation like Racket Mode, it will need to check
;; this -- is the definition location also a use of some other
;; definition? -- and follow that chain itself. Maybe we can provide a
;; convenience function for this. The point is, we don't try to bake
;; transitivity into the database -- we don't do any "resolve" or
;; "fixup" passes to update the db. And for uses, we don't need to
;; record both "source" and "nominal" IDs, and try to join using
;; either.
;;
;; The drawback? We do need some knowledge of the various way(s) in
;; which contract-out results in fully-expanded code, such as
;; expanding to either `make-provide/contract-transformer` or
;; `make-provide/contract-arrow-transformer`. Although I've reviewed
;; racket/contract/private/provide.rkt to confirm those are the two
;; possible expansions, there's no guarantee that won't change.
;;
;; Also, although a name "provide/contract-id-foo" is apparent in the
;; fully expanded syntax, that does NOT include the ".1" suffix. I
;; don't yet understand the rationale or mechanism behind the suffix.
;;
;; Better: Change racket/contract to supply a simpler canonical
;; expansion, i.e. an "official API" that promises to make this simple
;; and reliable.
(define (analyze-provide/contract-transformers path stx)

  (define (symbolic-compare? x y)
    (eq? (syntax-e x) (syntax-e y)))

  (define (handle-file-module stx)
    (syntax-case* stx (module) symbolic-compare?
      [(module _id _lang (_mb . es))
       (handle-module-level '() #'es)]))

  (define (handle-module-level submods es)
    (for ([e (in-syntax es)])
      (syntax-case* e
          (#%app
           define-syntaxes
           make-provide/contract-transformer
           make-provide/contract-arrow-transformer
           quote-syntax
           module
           module*)
          symbolic-compare?
        [(define-syntaxes
           (_)
           (#%app
            make-provide/contract-transformer
            (quote-syntax wrapper)
            (quote-syntax _)
            (quote-syntax original)
            . _))
         (add-def-for-contract path submods #'original #'wrapper)]
        [(define-syntaxes
           (_)
           (#%app
            make-provide/contract-arrow-transformer
            (quote-syntax wrapper)
            (quote-syntax _)
            (quote-syntax original)
            . _))
         (add-def-for-contract path submods #'original #'wrapper)]
        [(module id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [(module* id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [_ (void)])))

  (define (add-def-for-contract path submods original wrapper)
    (define beg (syntax-position original))
    (define span (syntax-span original))
    (cond
      [(and beg span)
       ;; FIXME: I don't yet understand where the ".1" suffix comes from;
       ;; fragile?!?
       (define wrapper-sym (string->symbol (~a (syntax-e wrapper) ".1")))
       (log-definitions-debug "contract add-def ~v ~v ~v ~v"
                              path submods (syntax-e original) wrapper-sym)
       (add-def path beg (+ beg span) submods wrapper-sym)]
      [else
       (log-definitions-warning "no syntax location for ~v in ~v"
                              (syntax-e original) path)]))

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
     (as uses.sym sym_id)
     (as (select str #:from strings #:where (= uses.sym strings.id)) sym_str)
     (as uses.defpath defpath_id)
     (as (select str #:from strings #:where (= uses.defpath strings.id)) defpath_str)
     (as defs.beg def_beg)
     (as defs.end def_end)
     #:from
     (left-join uses defs
                #:on (and (= uses.defpath defs.path)
                          (= uses.submods defs.submods)
                          (= uses.sym     defs.sym)))))))

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

(define (get-definition-submods-str sym path beg end)
  (query-maybe-value
   (select (select str #:from strings #:where (= id submods))
           #:from defs
           #:where (and (= sym ,(intern sym))
                        (= path ,(intern path))
                        (= beg ,beg)
                        (= end ,end)))))

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

;;; "commands"

;; Given a file position, see if it is a use of a definition. If so,
;; return a vector describing the definition location, else #f. i.e.
;; This is the basis for "find definition". One wrinkle here is that
;; we may already know about a use of a definition, and which file
;; defines it, but we haven't yet analyzed that defining file. See
;; comments below.
(define (use-pos->def-loc use-path pos)
  (let loop ([first-attempt? #t])
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
      ;; If we know it is a reference to a definition in a file, but
      ;; not the location within the file, it could mean we haven't
      ;; analyzed that other file yet. If so, analyze it now, then try
      ;; again. things:
      [(vector def-path subs (? db:sql-null?) (? db:sql-null?) (? db:sql-null?))
       (cond [(and first-attempt?
                   (analyze-path def-path))
              (loop #f)]
             [else (vector def-path subs #f 1 1)])]
      [(? vector? vec) vec]
      [_ #f])))

(define (use-pos->def-loc/transitive use-path pos)
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
   (match (use-pos->def-loc use-path pos)
     [(and vec (vector def-path _subs _sym (? integer? beg) _end))
      (loop vec def-path beg)]
     [(? vector? vec) vec]
     [#f previous-answer])))

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
                        (= sym     ,symid)))))

;; Given use of a def, return all uses of the def. i.e. "Find all
;; similar references".
(define (use-pos->def-and-use-locs use-path pos)
  (match (use-pos->def-loc use-path pos)
    [(and def-loc
          (vector def-path submods (? string? sym) _beg _end))
     (cons def-loc
           (get-uses def-path submods sym))]
    [_ #f]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serializing

(define (str v)
  (cond [(path? v)  (path->string v)]
        [(number? v) v]
        [else        (~a v)]))

(define (un-str s)
  (read (open-input-string s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples

(module+ in-memory-example
  (connect 'memory)
  (create-tables)
  (start-analyze-more-files-thread)
  (add-def "/a/b/c.rkt" 42 46 '(a b c) 'foo)
  (add-use "/d/e/f.rkt" 11 14 "/a/b/c.rkt" '(a b c) 'foo)
  (add-use "/d/e/f.rkt" 90 93 "/a/b/c.rkt" '(a b c) 'foo)
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
  (start-analyze-more-files-thread)

  ;; Re-analyze example/define.rkt and example/require.rkt.
  (define-runtime-path define.rkt "example/define.rkt")
  (define-runtime-path require.rkt "example/require.rkt")
  (forget-digest (build-path define.rkt))
  (forget-digest (build-path require.rkt))
  (analyze-path (build-path require.rkt))
  ;; Test that various uses in example/require.rkt point to the
  ;; correct definition location in example/define.rkt.
  (check-equal? (use-pos->def-loc require.rkt 42)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain" 88 93)
                "plain")
  (check-equal? (use-pos->def-loc require.rkt 48)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain" 88 93)
                "renamed")
  (check-equal? (use-pos->def-loc require.rkt 56)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted1.1" 207 218)
                "contracted1")
  (check-equal? (use-pos->def-loc require.rkt 68)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted2.1" 283 294)
                "contracted2")
  (check-equal? (use-pos->def-loc require.rkt 80)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted/renamed.1" 363 366)
                "contracted/renamed")
  (check-equal? (use-pos->def-loc require.rkt 99)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain-by-macro" 515 529)
                "plain-by-macro")
  (check-equal? (use-pos->def-loc require.rkt 114)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted-by-macro.1" 684 703)
                "contracted-by-macro")
  (check-equal? (use-pos->def-loc require.rkt 134)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "(sub)" "sub" 958 961)
                "sub")
  (check-equal? (use-pos->def-loc require.rkt 138)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "(sub)" "sub" 958 961)
                "sub/renamed")
  (check-equal? (use-pos->def-loc require.rkt 150)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "foo" 1179 1182)
                "foo")
  (check-equal? (use-pos->def-loc require.rkt 154)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "a-number" 1225 1233)
                "a-number")
  (check-equal? (use-pos->def-loc require.rkt 163)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "a-parameter" 1265 1276)
                "a-parameter")
  (check-equal? (use-pos->def-loc require.rkt 175)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-from-m.1" 1421 1427)
                "from-m")
  (check-equal? (use-pos->def-loc require.rkt 182)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "d/c" 1456 1459)
                "d/c")
  (check-equal? (use-pos->def-loc require.rkt 186)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "d/c" 1456 1459)
                "renamed-d/c")

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
