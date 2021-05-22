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
         racket/format
         racket/match
         racket/set
         sql
         "common.rkt")

(provide open
         close
         start-analyze-more-files-thread
         stop-analyze-more-files-thread
         analyze-path

         add-def
         add-arrow
         add-export-rename
         add-import-rename
         add-import
         add-export
         add-sub-range-binders
         add-mouse-over-status
         add-tail-arrow
         add-unused-require
         queue-more-files-to-analyze
         analyze-all-known-paths

         ;; High level queries
         use-pos->def/proximate
         use-pos->def/transitive
         def-pos->uses/proximate
         def-pos->uses/transitive
         find-def
         find-all-defs-named

         use-pos->name/proximate
         use-pos->name/transitive
         name-pos->uses/transitive

         ;; Low level queries.
         current-dbc
         ;; These are like the same-named `db` functions but lacking
         ;; the first, connection argument; current-dbc is used.
         query
         query-exec
         query-row
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
(define-db query-row)
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
  (define pid (intern path))
  (query-exec (delete #:from def_arrows      #:where (= use_path  ,pid)))
  (query-exec (delete #:from defs            #:where (= from_path ,pid)))
  (query-exec (delete #:from name_arrows     #:where (= use_path  ,pid)))
  (query-exec (delete #:from exports         #:where (= nom_path  ,pid)))
  (query-exec (delete #:from imports         #:where (= path      ,pid)))
  (query-exec (delete #:from mouseovers      #:where (= path      ,pid)))
  (query-exec (delete #:from tail_arrows     #:where (= path      ,pid)))
  (query-exec (delete #:from unused_requires #:where (= path      ,pid))))

(define (add-def path beg end subs symbol)
  (query-exec
   (insert #:into defs #:set
           [from_path ,(intern path)]
           [from_subs ,(intern subs)]
           [from_id   ,(intern symbol)]
           [beg       ,beg]
           [end       ,end]
           ;; FIXME: check-syntax will report identical
           ;; (path symbol subs) for a file-module-level
           ;; define and one inside a module+ form. :( I'd
           ;; expect subs to include the module+ name
           ;; but it does not. So for now simply ignore
           ;; any such shadowing definitions.
           #:or-ignore)))

(define (add-sub-range-binders subs srb)
  (let loop ([srb srb])
    (match srb
      [(cons this more)
       (loop this)
       (loop more)]
      [(or (vector use-stx sub-ofs sub-span
                   def-stx def-ofs def-span)
           (vector use-stx sub-ofs sub-span _ _
                   def-stx def-ofs def-span _ _))
       (define full-id (symbol->string (syntax->datum use-stx)))
       (define sub-id  (substring full-id sub-ofs (+ sub-ofs sub-span)))
       (define def-beg (+ (syntax-position def-stx) def-ofs))
       (define def-end (+ def-beg def-span))
       (query-exec
        (insert #:into sub_range_binders #:set
                [path     ,(intern (syntax-source def-stx))]
                [subs     ,(intern subs)]
                [full_id  ,(intern full-id)]
                [sub_ofs  ,sub-ofs]
                [sub_span ,sub-span]
                [sub_id   ,(intern sub-id)]
                [sub_beg  ,def-beg]
                [sub_end  ,def-end]
                #:or-ignore))]
      [_ (void)])))

;; Add an arrow to both the `def_arrows` and `name_arrows` tables.
(define (add-arrow use-path
                   use-beg use-end use-text use-stx
                   require-arrow
                   def-beg def-end def-text def-stx
                   from-path from-subs from-id
                   nom-path  nom-subs  nom-id
                   #:also-add-rename-arrow? [also-add-rename-arrow? #t])
  (add-def-arrow use-path
                 use-beg use-end use-text use-stx
                 require-arrow
                 def-beg def-end def-text def-stx
                 from-path from-subs from-id)
  (when also-add-rename-arrow?
    (add-name-arrow use-path
                    use-beg use-end use-text use-stx
                    require-arrow
                    def-beg def-end def-text def-stx
                    nom-path  nom-subs  nom-id)))

(define (add-def-arrow use-path
                       use-beg use-end use-text use-stx
                       require-arrow
                       def-beg def-end def-text def-stx
                       from-path from-subs from-id)
  (define kind (match require-arrow
                 [#f 0]
                 [#t 1]
                 ['module-lang 2]))
  (query-exec
   (insert #:into def_arrows
           #:set
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
           [from_path ,(intern/false->null from-path)]
           [from_subs ,(intern/false->null from-subs)]
           [from_id   ,(intern/false->null from-id)]
           #:or-ignore)))

(define (add-name-arrow use-path
                        use-beg use-end use-text use-stx
                        require-arrow
                        def-beg def-end def-text def-stx
                        nom-path  nom-subs  nom-id)
  ;; You would think we shouldn't add name_arrows between names that
  ;; don't match. And we don't, for /lexical/ name_arrows.
  ;;
  ;; However we /do/ add /require/ name_arrows where the names don't
  ;; match, because add-import-rename works by updating such
  ;; existing arrows in the table, after check-syntax has run.
  ;; (Probably this should be redesigned.)
  (when (or require-arrow (equal? (~a use-text) (~a def-text)))
    (define kind
      (match require-arrow
        [#f 0]
        ;; Treat use of prefix-in prefix as a lexical arrow to the
        ;; prefix (instead of a require arrow to the modpath).
        [#t (if (equal? (~a use-stx) (~a use-text nom-id)) 0 1)]
        ['module-lang 2]))
    (query-exec
     (insert #:into name_arrows
             #:set
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
             [nom_path  ,(intern/false->null nom-path)]
             [nom_subs  ,(intern/false->null nom-subs)]
             [nom_id    ,(intern/false->null nom-id)]
             ;; For things like `struct`, check-syntax might duplicate
             ;; syncheck:add-jump-to-definition.
             #:or-ignore))))

(define (intern/false->null v)
  (if v (intern v) db:sql-null))

(define (add-export-rename path subs old-stx new-stx)
  ;; Say that the rename is an additional use of the originally
  ;; defined thing.
  #;
  (println (list 'add-export-rename path subs old-stx new-stx))
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and new-beg new-end
             (not (equal? old-beg new-beg))
             (not (equal? old-end new-end)))
    (add-def-arrow path
                   new-beg new-end new-sym new-sym
                   #f ;lexical
                   (or old-beg new-beg) (or old-end new-end) old-sym old-sym
                   path subs old-sym)))

(define (add-import-rename path subs old-stx new-stx path-stx)
  #;
  (println (list 'add-import-rename path subs old-stx new-stx path-stx))
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and new-beg new-end
             (not (equal? old-beg new-beg))
             (not (equal? old-end new-end)))
    ;; Say that the rename is an additional use of the originally
    ;; defined thing.
    (add-def-arrow path
                   new-beg new-end new-sym new-sym
                   #f ;lexical
                   (or old-beg new-beg) (or old-end new-end) old-sym old-sym
                   path subs old-sym))
  ;; Given
  ;;
  ;;     (rename-in modpath [old new] [old2 new2])
  ;;     new
  ;;     new2
  ;;
  ;; or same with `only-in`: In the `name_arrows` table, only, update
  ;; any existing require arrows pointing to the same `modpath` and
  ;; having use_text = `new`, instead to be lexical arrows pointing to
  ;; the `new`. This assumes check-syntax has already run, and we need
  ;; to fixup some things add-arrow already added.
  (define-values (path-sym path-beg path-end) (stx->vals path-stx))
  (when (and new-beg new-end path-beg path-end)
    (query-exec
     (update name_arrows
             #:set
             [kind     0]
             [def_text ,(intern new-sym)]
             [def_beg  ,new-beg]
             [def_end  ,new-end]
             [use_text ,(intern new-sym)]
             [use_stx  ,(intern new-sym)]
             #:where (and (= kind 1)
                          (= use_text ,(intern new-sym))
                          (= use_path ,(intern path))
                          (= def_beg ,path-beg)
                          (= def_end ,path-end)))))
  ;; Also add arrow (in both tables) from `old` to `modpath`.
  (when (and old-beg old-end path-beg path-end)
    (define-values (from-path from-submods from-sym nom-path nom-submods nom-sym)
      (identifier-binding/resolved path old-stx 0 (syntax->datum old-stx)))
    (add-arrow path
               old-beg old-end old-sym old-sym
               #t ;require
               path-beg path-end path-sym path-sym
               from-path from-submods from-sym
               nom-path nom-submods nom-sym)))

(define (add-import path subs sym)
  (void)
  #;
  (query-exec
   (insert #:into imports #:set
           [path ,(intern path)]
           [subs ,(intern subs)]
           [sym  ,(intern sym)]
           #:or-ignore)))

(define (add-export path subs stx)
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (query-exec
        (insert #:into exports #:set
                [nom_path ,(intern path)]
                [nom_subs ,(intern subs)]
                [nom_id   ,(intern sym)]
                [beg      ,beg]
                [end      ,end]
                #:or-ignore))]
      [else ;#f beg and/or end
       ;; Assume this is a re-provide arising from all-from,
       ;; all-from-except, or all-from-out. The exported id has no
       ;; srcloc because it does not occur in the source. For the name
       ;; graph to work, we need to add this to `exports` and to
       ;; `name_arrows`. As this isn't actually in the source, we use
       ;; negative unique values for the positions. Things like
       ;; name-pos->uses/transitive can filter these.
       (match-define (vector use-beg use-end def-beg def-end)
         (query-row
          (select
           (- pos 3) (- pos 2) (- pos 1) (- pos 0)
           #:from
           (select (as (min use_beg) pos)
                   #:from (union (select (as 0 use_beg))
                                 (select use_beg
                                         #:from name_arrows
                                         #:where (= use_path ,(intern path)))
                                 #:all)))))
       (query-exec
        (insert #:into exports #:set
                [nom_path ,(intern path)]
                [nom_subs ,(intern subs)]
                [nom_id   ,(intern sym)]
                [beg      ,use-beg]
                [end      ,use-end]
                #:or-ignore))
       (define-values (_from-path _from-subs _from-sym nom-path nom-subs nom-sym)
         (identifier-binding/resolved path stx 0 #;level sym)) ;FIXME: phase level
       (add-name-arrow path
                       use-beg use-end sym sym
                       #t ;require arrow
                       def-beg def-end sym sym
                       nom-path nom-subs nom-sym)])))

(define (stx->vals stx)
  (define dat (syntax-e stx))
  (define beg (syntax-position stx))
  (define span (syntax-span stx))
  (define end (and beg span (+ beg span)))
  (values dat beg end))

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
;;; Queries

;; Given a file position, see if it is a use of a definition. If so,
;; return a vector describing the definition location, else #f. i.e.
;; This is the basis for "find definition". One wrinkle here is that
;; we may already know about a use of a definition, and which file
;; defines it, but we haven't yet analyzed that defining file. See
;; comments below.
(define (use-pos->def/proximate use-path pos #:retry? [retry? #t])
  (match (query-maybe-row
          (select (select str #:from strings #:where (= id def_path))
                  def_beg
                  def_end
                  #:from def_xrefs
                  #:where (and (= use_path ,(intern use-path))
                               (<= use_beg ,pos) (< ,pos use_end))))
    [(vector def-path (? integer? beg) (? integer? end))
     (vector def-path beg end)]
    [(vector def-path (== db:sql-null) (== db:sql-null))
     (cond [(and retry?
                 (analyze-path def-path))
            (use-pos->def/proximate use-path pos #:retry? #f)]
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
   (match (use-pos->def/proximate use-path pos)
     [(and vec (vector def-path def-beg _def-end))
      (if (equal? vec previous-answer)
          vec
          (loop vec def-path def-beg))]
     [#f previous-answer])))

(define (def-pos->uses/proximate path pos)
  (query-rows
   (select (select str #:from strings #:where (= strings.id use_path))
           (select str #:from strings #:where (= strings.id def_text))
           (select str #:from strings #:where (= strings.id use_text))
           (select str #:from strings #:where (= strings.id use_stx))
           use_beg
           use_end
           #:from def_xrefs
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
        (select x.def_path x.def_beg x.def_end
                x.use_path x.use_beg x.use_end
                x.def_text x.use_text x.use_stx
                #:from (as def_xrefs x)
                #:where (and (= def_path ,(intern path))
                             (<= def_beg ,pos)
                             (< ,pos def_end)))
        (select x.def_path x.def_beg x.def_end
                x.use_path x.use_beg x.use_end
                x.def_text x.use_text x.use_stx
                #:from (inner-join
                        rec (as def_xrefs x)
                        #:on
                        (and (= rec.use_path x.def_path)
                             (= rec.use_beg  x.def_beg)
                             (= rec.use_end  x.def_end)))))])
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

(define (use-pos->name/proximate use-path pos #:retry? [retry? #f])
  (match (query-maybe-row
          (select (select str #:from strings #:where (= id nom_path))
                  def_beg
                  def_end
                  #:from name_xrefs
                  #:where (and (= use_path ,(intern use-path))
                               (<= use_beg ,pos) (< ,pos use_end))
                  #:limit 1))
    [(vector def-path (? integer? beg) (? integer? end))
     (vector def-path beg end)]
    [(vector nom-path (== db:sql-null) (== db:sql-null))
     (cond [(and retry?
                 (analyze-path nom-path))
            (use-pos->def/proximate use-path pos #:retry? #f)]
           [else
            (vector nom-path 1 1)])]
    [#f #f]))

;; Like use-pos->name, but when the name loc is also a use of another
;; name return that.
(define (use-pos->name/transitive use-path pos)
  ;; Although we could use a recursive CTE here, we want to use the
  ;; ability of use-pos->name to call analyze on-demand (which we can't
  ;; do inside a SQL query). Anyway, the potential performance benefit
  ;; here is less -- we're "drilling down" linearly to one answer, not
  ;; "fanning out" as with name-pos->uses/transitive.
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
   (match (use-pos->name/proximate use-path pos)
     [(and vec (vector def-path def-beg _def-end))
      (if (equal? vec previous-answer)
          vec
          (loop vec def-path def-beg))]
     [#f previous-answer])))

(define (name-pos->uses/transitive path pos)
  (query-rows
   (sql
    (with
     #:recursive
     ([(rec nom_path def_beg def_end
            use_path use_beg use_end
            nom_id   use_text use_stx)
       (union
        (select x.nom_path x.def_beg x.def_end
                x.use_path x.use_beg x.use_end
                x.nom_id   x.use_text x.use_stx
                #:from (as name_xrefs x)
                #:where (and (= nom_path ,(intern path))
                             (<= def_beg ,pos)
                             (< ,pos def_end)))
        (select x.nom_path x.def_beg x.def_end
                x.use_path x.use_beg x.use_end
                x.nom_id   x.use_text x.use_stx
                #:from (inner-join
                        rec (as name_xrefs x)
                        #:on
                        (and (= rec.use_path x.nom_path)
                             (= rec.use_beg  x.def_beg)
                             (= rec.use_end  x.def_end)
                             (= rec.use_text x.use_text)
                             (= rec.nom_id   x.nom_id)))))])
     (select (select str #:from strings #:where (= id use_path))
             (select str #:from strings #:where (= strings.id nom_id))
             (select str #:from strings #:where (= strings.id use_text))
             (select str #:from strings #:where (= strings.id use_stx))
             use_beg
             use_end
             #:from rec
             #:order-by use_path use_beg
             ;; ignore all-from-out "dummy" nodes:
             #:where (and (< 0 use_beg) (< 0 use_end)))))))

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

(define (analyze-more-files)
  (define (on-more-files paths)
    (define n (set-count paths))
    (log-pdb-debug
     "analyze-more-files-thread got ~v more files to check: ~v" n paths)
    (set-for-each paths analyze-path)
    (log-pdb-debug
     "analyze-more-files-thread analyzed or skipped ~v files" n)
    (analyze-more-files))
  (sync (handle-evt stop-ch void)             ;exit thread
        (handle-evt todo-ach on-more-files))) ;recur

(define (start-analyze-more-files-thread)
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
