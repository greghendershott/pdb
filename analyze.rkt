;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require drracket/check-syntax
         openssl/sha1
         racket/class
         racket/contract
         racket/file
         racket/format
         (only-in racket/list remove-duplicates)
         racket/logging
         racket/match
         racket/path
         (only-in racket/port open-output-nowhere)
         racket/phase+space
         syntax/modread
         syntax/id-table
         "analyze-more.rkt"
         "common.rkt"
         "data-types.rkt"
         "extra-arrows.rkt"
         (prefix-in cache: "cache.rkt")
         (prefix-in store: (only-in "store.rkt" get-digest)))

(provide get-file
         analyze-path
         fresh-analysis?
         fresh-analysis-expanded-syntax
         add-directory
         forget-path
         forget-directory)

(struct analysis (file))
(struct cached-analysis analysis ())
(struct fresh-analysis analysis (import-paths expanded-syntax))

(define/contract (get-file path)
  (-> (and/c path? complete-path?) file?)
  (match (cache:get-file path)
    [(? file? f) f]
    [#f
     (define ch (make-channel))
     (spawn-do-analyze-path path
                            #:code         #f
                            #:always?      #f
                            #:import-depth 0
                            #:result-chan  ch)
     (match (sync ch)
       [(? exn? e)      (raise e)]
       [(? analysis? v) (analysis-file v)])]))

;; Intended for an editor tool to call whenever the user has made
;; changes.
;;
;; Supplying a #:code string allows analyzing changes not saved to a
;; file on disk. In this case you must still supply some
;; complete-path?, even if it is for a file that does not (yet) exist.
;;
;; Whether by using #:code or the contents of `path`, read-syntax and
;; expand are used in the usual way. drracket/check-syntax and some
;; additional analysis are done. The results are stored in a db.
;;
;; #:import-depth says how many levels to recur and analyze imported
;; files, too. Reasonable values are 0 (none), 1 (just files directly
;; imported by the analyzed file, on the theory the user might access
;; one of them soon), or a very large integer (all that can be found
;; transitively, up to "opaque" modules like #%core and #%kernel).
;; Other values are legal but of dubious value. The analysis of these
;; imported files takes place on another thread, and does not
;; meaningfully delay returning results for the primary file.
;;
;; #:always? is relevant only when #:code is false; it says even if we
;; have an analysis for a file with a digest matching what's on disk,
;; to re-analyze it anyway. TL;DR: ignore any cached result, even if
;; apparently still valid. This is mainly intended for internal use
;; while developing this library, for debugging and for its unit
;; tests.
;;
;; Returns one of:
;;
;; - An opaque struct value #<cached-result> meaning that an analysis
;;   of `path` matching its digest already existed in the db and
;;   nothing needed to be done.
;;
;; - An opque struct value satisfying the predicate fresh-analysis?,
;;   meaning that a suitable analysis did not already exist -- or you
;;   supplied non-false #:always? -- and a fresh analysis was done. In
;;   this case, the value may be given to
;;   fresh-analysis-expanded-syntax to get the fully expanded syntax.
;;
;; - An exn:break?, meaning that after you called analyze-path for a
;;   given `path`, another thread later called it for the same path.
;;   The older thread exits rather than continue doing work, and the
;;   exn:break value tells you this is what happened and there is no
;;   immediate result.
;;
;; - If an exception occurs during expansion, the resulting error(s)
;;   are available by calling `get-errors`. If another exception
;;   occurs, then it will be raised in your calling thread.
(define/contract (analyze-path path
                               #:code         [code #f]
                               #:import-depth [import-depth 0]
                               #:always?      [always? #f])
  (->* ((and/c path? complete-path?))
       (#:code         (or/c #f string?)
        #:import-depth exact-nonnegative-integer?
        #:always?      boolean?)
       (or/c analysis? exn:break?))
  (define ch (make-channel))
  (spawn-do-analyze-path path
                         #:code         code
                         #:always?      always?
                         #:import-depth import-depth
                         #:result-chan  ch)
  (match (sync ch)
    [(? exn:fail? e)  (raise e)]
    [(? exn:break? e) e]
    [(? analysis? v)  v]))

;;; Worker thread

;; We support being called from multiple client threads. For each
;; call, we create a single worker thread, so that we can break-thread
;; and abandon an old analysis still underway if asked by another
;; client thread to analyze the same file. (Scenario: Client called us
;; after user made an edit. We start analyzing. User makes another
;; edit. The old analysis is now outdated.)
(define spawn-do-analyze-path
  (let ([sema (make-semaphore 1)] ;guard concurrent use of ht
        [ht   (make-hash)])       ;path? => thread?
    (λ (path
        #:code         [code #f]
        #:always?      [always? #f]
        #:import-depth [max-depth 0]
        #:result-chan  [result-chan #f]      ;to block for analysis? or exn? result
        #:imports-chan [imports-chan #f]     ;to block until all imports done
        #:done-paths   [done (mutable-set)]) ;to avoid even sha1 compares
      (define (do-analyze-thunk)
        (set-add! done path)
        (define result (do-analyze-path path
                                        #:code     code
                                        #:always?  always?
                                        #:exp-stx? #t))
        (when result-chan (channel-put result-chan result))
        (when (fresh-analysis? result)
          (define (do-path path)
            (define result (do-analyze-path path
                                            #:code     #f
                                            #:always?  #f
                                            #:exp-stx? #f))
            (if (fresh-analysis? result)
                (fresh-analysis-import-paths result)
                (set)))
          (define (do-paths paths)
            (define more (mutable-set))
            (set-subtract! paths done)
            (for ([path (in-set paths)])
              (set-add! done path)
              (set-union! more (do-path path)))
            more)
          (for/fold ([imports (fresh-analysis-import-paths result)])
                    ([depth (in-range 1 (add1 max-depth))]
                     #:when (not (set-empty? imports)))
            (log-pdb-info "depth ~v: ~v imports to analyze ..." depth (set-count imports))
            (do-paths imports)))
        (call-with-semaphore sema (λ () (hash-remove! ht path)))
        (when imports-chan (channel-put imports-chan #t)))
      ;; break-thread any existing thread for the same path (supports
      ;; user making frequent edits; we need't complete potentially
      ;; lengthy analysis that's no longer relevant), then start a new
      ;; thread for this analyzsis.
      (call-with-semaphore
       sema
       (λ ()
         (define maybe-old-thread-for-path (hash-ref ht path #f))
         (when maybe-old-thread-for-path
           (log-pdb-debug "breaking already running thread for ~v" path)
           (break-thread maybe-old-thread-for-path))
         (hash-set! ht path (thread do-analyze-thunk)))))))

;; Analyze all files in and under `dir`. The optional #:import-depth
;; and #:always? args are the same as for `analyze-path`.
(define/contract (add-directory dir
                                #:import-depth [import-depth 0]
                                #:always?      [always? #f])
  (->* ((and/c path? complete-path?))
       (#:import-depth exact-nonnegative-integer?
        #:always?      boolean?)
       any)
  (log-pdb-info "add-directory ~v" dir)
  (define (use-dir? d)
    (not (member (file-name-from-path d)
                 (map build-path '("compiled" ".trash")))))
  (define done (mutable-set))
  (for ([path (in-directory dir use-dir?)])
    (when (equal? (path-get-extension path) #".rkt")
      ;; Avoid work by checking digest ASAP. (In contrast to get-file
      ;; and analyze-path where we want to read the `file` struct when
      ;; the digest matches, here we want to do nothing.)
      (define code (file->string path #:mode 'text))
      (define stored-digest (store:get-digest path))
      (when (or always?
                (not stored-digest)
                (not (equal? stored-digest (sha1 (open-input-string code)))))
        ;; Although we could spawn N threads here, let's wait for each
        ;; to complete, by sync-ing using #:imports-chan.
        (define ch (make-channel))
        (spawn-do-analyze-path (simple-form-path path)
                               #:code         code
                               #:import-depth import-depth
                               #:always?      always?
                               #:done-paths   done
                               #:imports-chan ch)
        (sync ch)))))

(define (forget-paths paths)
  (for ([path (in-set paths)])
    (cache:forget (simple-form-path path))))

(define/contract (forget-path path)
  (-> (and/c path? complete-path?) any)
  (forget-paths (list path)))

(define/contract (forget-directory path)
  (-> (and/c path? complete-path?) any)
  (forget-paths (find-files values path)))

;;; Analysis per se

(struct analyzing (path file sub-range-binders re-exports) #:transparent)

(define current-analyzing (make-parameter #f)) ;(or/c #f analyzing?)
(define (get path)
  (match (current-analyzing)
    [(analyzing (== path) (? file? f) _sub-range-binders _re-exports) f]
    [v (error 'get
              "called for path ~v but currently analyzing ~v"
              path v)]))

(define (do-analyze-path path
                         #:code     code-str
                         #:always?  always?
                         #:exp-stx? exp-stx?)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-pdb-warning "error analyzing ~v:\n~a" path (exn->string e))
                     (forget-path path)
                     e)]
                  [exn:break?
                   (λ (e)
                     (log-pdb-debug "got exn:break for ~v" path)
                     e)])
    (define code (or code-str (file->string path #:mode 'text)))
    (define digest (sha1 (open-input-string code)))
    (define orig-f (cache:get-file path digest))
    (cond
      [(or always?
           (not orig-f))
       (define f (make-file))
       (parameterize ([current-analyzing (analyzing path
                                                    f
                                                    (make-free-id-table)
                                                    (mutable-set))])
         (with-time/log (~a "total " path)
           (log-pdb-info (~a "analyze " path " ..."))
           (match-define (cons imports exp-stx) (analyze-code path code))
           (with-time/log "add our arrows"
             (file-add-arrows f))
           (let ([f (maybe-copy-imports-from-cached-analysis path f)])
             (with-time/log "update db"
               (cache:put path f digest
                          #:exports
                          (for*/list ([(ibk srs) (in-hash (file-pdb-exports f))]
                                      [sr (in-list srs)])
                            (define pos
                              (match (sub-range-sub-pos sr)
                                [(? number? pos) pos]
                                [_ #f]))
                            (list ibk
                                  (sub-range-offset sr)
                                  (sub-range-span sr)
                                  (sub-range-sub-sym sr)
                                  pos))
                          #:re-exports
                          (analyzing-re-exports (current-analyzing))
                          #:imports
                          (for/list ([a (in-list (arrow-map-arrows (file-arrows f)))]
                                     #:when (import-arrow? a))
                            (list (car (import-arrow-nom a))
                                  (cdr (import-arrow-nom a))
                                  (arrow-use-beg a)
                                  (arrow-use-end a)))))
             (fresh-analysis f imports (and exp-stx? exp-stx)))))]
      [else
       (cached-analysis orig-f)])))

;; If the analysis had errors and there are no imports (used to supply
;; completion candidates), then copy them from previous analysis in
;; cache, if any. It's fine to look just in cache because the use case
;; here is an end user typing, having some error, and wanting to have
;; candidates available while they correct the error. So I think
;; there's no need to read from db storage, decompress and
;; deserialize, just to get imports.
;;
;; We do not copy the old file-pdb-modules; the source positions won't
;; correspond. The get-completion-candidates function in query.rkt can
;; in that case simply supply a union of everything in
;; file-pdb-imports (all imports for all modules).
(define (maybe-copy-imports-from-cached-analysis path f)
  (if (and (not (span-map-empty? (file-pdb-errors f)))
           (dict-empty? (file-pdb-imports f)))
      (let ([previous-f (cache:get-file path)])
        (if previous-f
            (struct-copy file f
                         [pdb-imports (file-pdb-imports previous-f)])
            f))
      f))

(define (analyze-code path code-str)
  ;; (-> (and/c path? complete-path?) string?
  ;;     (cons/c (set/c path?) (or/c #f syntax?)))
  (define stx
    (with-module-reading-parameterization
      (λ ()
        (define (handle-exn:fail:read e)
          (match (reverse ((exn:srclocs-accessor e) e)) ;most-specific
            [(cons (srcloc (? path? error-path) _ _ (? number? pos) (? number? span)) _)
             (add-error path error-path pos (+ pos span) (exn-message e))]
            [_
             (add-error path path 1 2 (exn-message e))])
          #'"")
        (define in (open-input-string code-str path))
        (port-count-lines! in)
        (parameterize ([current-directory-for-user (find-system-path 'pref-dir)])
          (match (with-handlers ([exn:fail:read? handle-exn:fail:read])
                   (read-syntax path in))
            [(? syntax? stx) stx]
            [(? eof-object?) #'""])))))
  (define dir (path-only path))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load-relative-directory dir]
                 [current-directory               dir])
    (define exp-stx
      (with-time/log (~a "expand " path)
        (expand/gather-errors-and-mouse-overs stx path code-str)))
    (cond
      [exp-stx
       (define import-paths
         (with-time/log (~a "check-syntax " path)
           (analyze-using-check-syntax path exp-stx code-str)))
       (with-time/log (~a "analyze-more " path)
         (analyze-more add-module
                       add-definitions
                       (add-export code-str)
                       add-imports
                       (add-import-rename code-str)
                       path
                       exp-stx))
       (cons import-paths exp-stx)]
      [else
       (cons (set) #f)])))

(define (expand/gather-errors-and-mouse-overs stx src-path code-str)
  ;; 1. There are quite a few nuances wrt gathering error messages.

  ;; Typed Racket can report multiple errors. The protocol: it calls
  ;; error-display-handler for each one. There is a final, actual
  ;; exn:fail:syntax raised, but it's not useful for us: Although its
  ;; srclocs correspond to the locations, its message is just a
  ;; summary. Here we collect each message and location, and when the
  ;; final summary exn is raised, we ignore it. Note that Typed Racket
  ;; is the only such example I'm aware of, but if something else
  ;; wanted to report multiple errors, and it used a similar approach,
  ;; we'd handle it here, too.
  (define error-display-handler-called-before-exn? #f)
  (define (our-error-display-handler msg exn)
    (when (and (exn:fail:syntax? exn)
               (exn:srclocs? exn))
      (set! error-display-handler-called-before-exn? #t)
      (exn-with-srclocs exn)))

  (define (handle-fail e)
    (cond
      ;; See comment above. The final exn-message is unhelpful; ignore.
      [error-display-handler-called-before-exn?
       (void)]
      ;; The intended use of exn:srclocs is a _single_ error, with zero
      ;; or more locations from least to most specific -- not multiple
      ;; errors.
      [(exn:srclocs? e)
       (exn-with-srclocs e)]
      ;; A single error with no srcloc at all. Although probably
      ;; unlikely with exn:fail:syntax during expansion (as opposed to
      ;; runtime errors) do handle it:
      [else
       (exn-without-srclocs e)])
    #f)

  (define (exn-with-srclocs e)
    (match ((exn:srclocs-accessor e) e)
      [(list)
       (match e
         ;; exn:fail:syntax and exn:fail:read can have empty srclocs
         ;; list -- but additional struct member has list of syntaxes
         ;; from least to most specific. Use the most-specific, only.
         [(or (exn:fail:syntax msg _marks (list _ ... stx))
              (exn:fail:read   msg _marks (list _ ... stx)))
          #:when (not (exn:fail:read:eof? e))
          (define pos  (syntax-position stx))
          (define span (syntax-span stx))
          (cond [(and pos span)
                 (add-error src-path
                            (or (syntax-source stx) src-path)
                            pos
                            (+ pos span)
                            msg)]
                [else (exn-without-srclocs e)])]
         [_ (exn-without-srclocs e)])]
      [(list _ ... (? srcloc? most-specific))
       (match-define (srcloc error-path _ _ pos span) most-specific)
       (add-error src-path error-path pos (+ pos span) (exn-message e))]
      [_ (exn-without-srclocs e)]))

  (define (exn-without-srclocs e)
    ;; As a fallback, here, we extract location from the exn-message.
    ;; Unfortunately that has line:col. We need [beg end).
    (define-values (error-path pos) (exn-message->path&pos (exn-message e)))
    (add-error src-path error-path pos (add1 pos) (exn-message e)))

  (define (exn-message->path&pos msg)
    (match msg
      [(pregexp "^(.+?):(\\d+)[:.](\\d+): "
                (list _ error-path (app string->number line) (app string->number col)))
       (define in (open-input-string code-str))
       (port-count-lines! in)
       (values error-path
               (let loop ([n 1])
                 (cond [(= n line)                   (+ 1 (file-position in) col)]
                       [(eof-object? (read-line in)) 1]
                       [else                         (loop (add1 n))])))]
      [_ (values src-path 1)]))

  (define (do-expand)
    (parameterize
        ([error-display-handler our-error-display-handler]
         ;; We want complete paths in error messages. srcloc->string
         ;; from racket/base uses current-directory-for-user to elide
         ;; paths. Prevent that by setting to 'pref-dir (a very
         ;; unlikely location for a user's source file).
         [current-directory-for-user (find-system-path 'pref-dir)]
         [current-output-port (open-output-nowhere)]
         [current-error-port (open-output-nowhere)])
      (with-handlers ([exn:fail? handle-fail])
        (expand stx))))

  ;; 2. There exists a protocol for macros to communicate tooltips to
  ;; DrRacket via a log-message to the logger 'online-check-syntax.
  ;; Although this might seem strange, the motivation is that e.g. a
  ;; type-checker might learn things during expansion that it would
  ;; like to show the user -- even if expansion fails.
  (define (on-logger-event event)
    (match-define (vector _level _message stxs _topic) event)
    (for ([stx (in-list stxs)])
      (let walk ([v (syntax-property stx 'mouse-over-tooltips)])
        (match v
          ;; "The value of the 'mouse-over-tooltips property is
          ;; expected to be to be a tree of cons pairs (in any
          ;; configuration)..."
          [(cons v more)
           (walk v)
           (walk more)]
          ;; "...whose leaves are either ignored or are vectors of the
          ;; shape:"
          [(vector (? syntax? stx)
                   (? exact-nonnegative-integer? beg)
                   (? exact-nonnegative-integer? end)
                   (or (? string? string-or-thunk)
                       (? procedure? string-or-thunk)))
           (when (equal? src-path (syntax-source stx))
             (define (force v) (if (procedure? v) (v) v))
             (define str (force string-or-thunk))
             (span-map-add! (file-syncheck-mouse-overs (get src-path))
                            (add1 beg)
                            (add1 end)
                            str))]
          ;; Expected; quietly ignore
          [(or (list) #f) (void)]
          ;; Unexpected; log warning and ignore
          [v (log-pdb-warning "unknown online-check-syntax ~v" v)
             (void)]))))
  (with-intercepted-logging
    on-logger-event
    do-expand
    'info 'online-check-syntax))

(define (add-error src-path error-path beg end msg)
  #;(println `(add-error ,src-path ,error-path ,beg ,end ,msg))
  (span-map-add! (file-pdb-errors (get src-path))
                 beg
                 end
                 (cons (if (equal? src-path error-path)
                           #f
                           error-path)
                       msg)))

;; A wrapper to pretend two syntax objects are equal when they have
;; the same syntax-source. Used in our syncheck:find-source-object and
;; syncheck:add-arrow methods, below.
;;
;; <https://racket.discourse.group/t/syncheck-find-source-object/1829/1>
(struct wrapper (stx)
  #:methods gen:equal+hash
  [(define (equal-proc a b _recur)
     (equal? (syntax-source (wrapper-stx a))
             (syntax-source (wrapper-stx b))))
   (define (hash-proc v _recur)
     (equal-hash-code (syntax-source (wrapper-stx v))))
   (define (hash2-proc v _recur)
     (equal-secondary-hash-code (syntax-source (wrapper-stx v))))])

;; Note: drracket/check-syntax reports zero-based positions but we use
;; one-based (as for syntax-position).
(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (field [imported-files (mutable-set)])

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           (wrapper stx)))

    (define/override (syncheck:add-definition-target/phase-level+space
                      _so beg end sym rev-mods phase)
      (hash-set! (file-syncheck-definition-targets (get src))
                 (ibk rev-mods phase sym)
                 (cons (add1 beg) (add1 end))))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                      _so beg end sym path mods phase)
      (span-map-set! (file-syncheck-jumps (get src))
                     (add1 beg)
                     (add1 end)
                     (syncheck-jump sym path mods phase)))

    (define/override (syncheck:add-prefixed-require-reference
                      _req-so req-pos-left req-pos-right prefix
                      _prefix-so prefix-left prefix-right)
      (span-map-set! (file-syncheck-prefixed-requires (get src))
                     (add1 prefix-left)
                     (add1 prefix-right)
                     (syncheck-prefixed-require-reference
                      prefix
                      (add1 req-pos-left)
                      (add1 req-pos-right))))

    (define/override (syncheck:add-arrow/name-dup/pxpy
                      def-so def-beg def-end def-px def-py
                      use-so use-beg use-end use-px use-py
                      actual? phase require-arrow _name-dup?)
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define use-stx (wrapper-stx use-so))
      (define rb (identifier-binding/resolved src use-stx phase))
      (set-add! (file-syncheck-arrows (get src))
                (syncheck-arrow (add1 def-beg) (add1 def-end) def-px def-py
                                (add1 use-beg) (add1 use-end) use-px use-py
                                actual? phase require-arrow
                                use-sym def-sym rb)))

    (define/override (syncheck:add-require-open-menu _so beg end filename)
      (set-add! imported-files filename)
      (span-map-set! (file-syncheck-require-opens (get src))
                     (add1 beg)
                     (add1 end)
                     filename))

    (define/override (syncheck:add-mouse-over-status _so beg end str)
      ;; Note: There may exist multiple mouse-over messages for the same
      ;; span, such as both "imported from X" and "N binding
      ;; occurrences". We use span-map-add! not -set! here.
      (span-map-add! (file-syncheck-mouse-overs (get src))
                     (add1 beg)
                     (add1 end)
                     str))

    (define/override (syncheck:add-tail-arrow _from-so from-pos _to-so to-pos)
      ;; Our find-source-object insists on both matching analyzed source.
      (unless (= from-pos to-pos)
        (set-add! (file-syncheck-tail-arrows (get src))
                  (cons (add1 from-pos)
                        (add1 to-pos)))))

    (define/override (syncheck:add-docs-menu _so beg end sym label path anchor anchor-text)
      (span-map-set! (file-syncheck-docs-menus (get src))
                     (add1 beg)
                     (add1 end)
                     (syncheck-docs-menu sym label path anchor anchor-text)))

    (define/override (syncheck:add-unused-require _so beg end)
      (span-map-set! (file-syncheck-unused-requires (get src))
                     (add1 beg)
                     (add1 end)
                     #t))

    (define/override (syncheck:add-text-type _so beg end type)
      (span-map-set! (file-syncheck-text-types (get src))
                     (add1 beg)
                     (add1 end)
                     type))

    (super-new)))

;; Returns direct imports discovered during analysis
(define (analyze-using-check-syntax path exp-stx code-str)
  (parameterize ([current-annotations (new annotations-collector%
                                           [src path]
                                           [code-str code-str])])
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace)
                      (current-load-relative-directory)))
    (expanded-expression exp-stx)
    (expansion-completed)
    (get-field imported-files (current-annotations))))

(define (add-definitions def-stx mods phase+space id-stxs)
  #;(println (list 'add-definitions def-stx mods phase+space id-stxs))
  (define srbs (syntax-property def-stx 'sub-range-binders))
  (when srbs
    (add-sub-range-binders srbs))
  (define f (get (analyzing-path (current-analyzing))))
  (for ([id (in-list (if (list? id-stxs) id-stxs (syntax->list id-stxs)))])
    ;; Use identifier-binding here both to filter out lexical
    ;; bindings, and, to determine the actual symbolic name (which
    ;; matters with things like contract wrappers).
    (define ib (identifier-binding id (phase+space-phase phase+space)))
    (when (and (list? ib)
               (syntax-source id)
               (syntax-position id)
               (syntax-span id))
      (define sym (list-ref ib 1)) ;instead of (syntax-e id)
      (hash-set! (file-pdb-definitions f)
                 (ibk mods phase+space sym)
                 (free-id-table-ref (analyzing-sub-range-binders (current-analyzing))
                                    id
                                    (list
                                     (sub-range 0
                                                (syntax-span id)
                                                (syntax-e id)
                                                (syntax-position id))))))))

(define (add-sub-range-binders srbs)
  #;(println (list 'add-sub-range-binders mods phase srbs))
  (let loop ([v srbs])
    (match v
      [(cons this more)
       (loop this)
       (loop more)]
      [(or (vector full-stx full-ofs full-span
                   sub-stx sub-ofs sub-span)
           (vector full-stx full-ofs full-span _ _
                   sub-stx sub-ofs sub-span _ _))
       (unless (= full-span sub-span)
         (log-pdb-warning "full-span ~v does not equal sub-span ~v in full-stx ~v and sub-stx ~v"
                          full-span sub-span full-stx sub-stx))
       (define path (syntax-source sub-stx))
       (when (and path
                  ;; TODO: Not sure how to handle when different.
                  ;; Arises with e.g.
                  ;; <pkgs>/redex-benchmark/redex/benchmark/models/rvm/rvm-14.rkt.
                  (equal? path (analyzing-path (current-analyzing)))
                  (path-string? path)
                  (identifier? full-stx)
                  (identifier? sub-stx)
                  (syntax-position full-stx)
                  (syntax-position sub-stx))
         (free-id-table-update! (analyzing-sub-range-binders (current-analyzing))
                                full-stx
                                (λ (vs)
                                  (remove-duplicates
                                   (cons (sub-range full-ofs
                                                    full-span
                                                    (syntax-e sub-stx)
                                                    (+ (syntax-position sub-stx) sub-ofs))
                                         vs)))
                                null))]
      [_ (void)])))

;; This uses the sort of value for the syntax property requested by
;; rfindler -- where it supplies the full identifier syntax, so that
;; someday it could be attached to something other than the identifier
;; syntax it describes:
;; https://github.com/racket/racket/pull/4649#issuecomment-1576770816
;;
;; But meanwhile it is always directly attached -- which is good
;; because here we really do want to deal with the property value in
;; conjunction with the `rename` in which the id stx lives. Rather
;; than use a side free id table or other tactics that aren't yet
;; needed (and might not even be exercised/tested effectively), for
;; now we check and assert the direct attachment.
(define (syntax-import-or-export-prefix-ranges stx)
  (match (syntax-property stx 'import-or-export-prefix-ranges)
    [(vector full-id ranges)
     (unless (free-identifier=? full-id stx)
       (error 'syntax-import-or-export-prefix-ranges
              "\n  expected syntax property to be attached to the same identifier syntax:\n    ~v\n  as the full identifier in the property value:\n    ~v"
              stx
              full-id))
     (for/list ([v (in-list ranges)])
       (match-define (vector offset span sub-stx) v)
       (sub-range offset span (syntax-e sub-stx) (syntax-position sub-stx)))]
    [#f #f]))

(define ((add-export code-str) path mods phase+space export-id [local-id export-id])
  #;(println (list 'add-export path mods phase+space export-id local-id))
  (define f (get path))

  (define export-ibk (ibk mods phase+space (syntax-e export-id)))

  ;; export-id might be composed from other identifiers, on two levels:
  ;;
  ;; First, an 'import-or-export-prefix-ranges property might reveal
  ;; one or more prefixes, as well as the final suffix.
  ;;
  ;; Next, the final suffix, corresponding to the local-id, might have
  ;; sub-range-binders.
  ;;
  ;; Sometimes both: e.g. (prefix-out (struct-out)).
  ;;
  ;; Sometimes neither: Even when export-id and local-id differ, i.e.
  ;; there was a #%provide #%rename clause, that might originate from
  ;; something like a surface provide rename-out.
  ;;
  ;; So first normalize this to a list of ranges. In the simplest case
  ;; (no prefixes or sub-range-binders), there is a single range for
  ;; the entire export-id. In the most complicated case, the prefix
  ;; ranges and sub-range-binders are concatenated, with the latter
  ;; being shifted.
  (define appended-ranges
    (match* [(or (syntax-import-or-export-prefix-ranges export-id)
                 null)
             (free-id-table-ref (analyzing-sub-range-binders (current-analyzing))
                                local-id
                                null)]
      [[(list) (list)]
       (list
        (sub-range 0
                   (or (syntax-span export-id)
                       (string-length (symbol->string (syntax-e export-id))))
                   (syntax-e export-id)
                   (syntax-position export-id)))] ;might be #f, see below
      [[pres (list)] pres]
      [[(list) srbs] srbs]
      [[(list pres ... subsumed) srbs]
       ;; Append all but the last prefix range with all the
       ;; sub-range-binders shifted to replace the last prefix.
       (append pres
               (for/list ([srb (in-list srbs)])
                 (sub-range (+ (sub-range-offset srb) (sub-range-offset subsumed)) ;shift
                            (sub-range-span srb)
                            (sub-range-sub-sym srb)
                            (sub-range-sub-pos srb))))]))
  ;; When the last (maybe only) range is local-id but with a
  ;; non-lexical identifier-binding, we assume it is a re-export. If
  ;; it also lacks syntax-position, we assume it is an anonymous
  ;; re-export via all-from-out.
  ;;
  ;; Also we update a hash-table for all re-exports (anonymous or not)
  ;; used to build a db table to speed finding uses of
  ;; definitions/exports.
  (define adjusted-ranges
    (match appended-ranges
      [(list pres ... (sub-range ofs span (== (syntax-e local-id)) sub-pos))
       (match (identifier-binding/resolved path local-id (phase+space-phase phase+space))
         [(? resolved-binding? rb)
          (define nom-path (resolved-binding-nom-path rb))
          (define nom-ibk (ibk (resolved-binding-nom-subs rb)
                               (resolved-binding-nom-export-phase+space rb)
                               (resolved-binding-nom-sym rb)))
          ;; Consider it a RE-export if any of the path, modules, or
          ;; phase differ (but not if just the syms differ).
          (unless (and (equal? nom-path path)
                       (equal? (ibk-mods nom-ibk) (ibk-mods export-ibk))
                       (equal? (ibk-phase nom-ibk) (ibk-phase export-ibk)))
            (set-add! (analyzing-re-exports (current-analyzing))
                      (list nom-path nom-ibk
                            ofs span
                            path export-ibk)))
          (cond [sub-pos
                 appended-ranges]
                [else
                 (append pres
                         (list
                          (sub-range ofs
                                     span
                                     (syntax-e local-id)
                                     (re-export nom-path nom-ibk))))])]
         [#f
          (log-pdb-warning "could not find identifier-binding for ~v" local-id)
          appended-ranges])]
      [_
       appended-ranges]))
  ;; Add the export only when all the sub-ranges' srcloc makes sense.
  ;; Avoid errors and omissions from macros. One example: struct's
  ;; sub-range-binders neglect to override the `struct:id` identifier
  ;; (for the "structure type descriptor"), leaving it with bogus
  ;; srcloc.
  (define (sub-range-valid-srcloc? sr)
    (match-define (sub-range _ofs span sub-sym sub-pos) sr)
       (or (re-export? sub-pos)
           (and (number? sub-pos)
                (let* ([beg (sub1 sub-pos)]
                       [end (+ beg span)])
                  (define (valid-ix ix)
                    (and (<= 0 ix) (< ix (string-length code-str))))
                  (and (valid-ix beg)
                       (valid-ix end)
                       (equal? (substring code-str beg end)
                               (symbol->string sub-sym)))))))
  (cond
    [(andmap sub-range-valid-srcloc? adjusted-ranges)
     (hash-set! (file-pdb-exports f) export-ibk adjusted-ranges)]
    [else
     (log-pdb-warning "ignoring export due to bogus source locations: ~v ~v"
                      export-ibk
                      adjusted-ranges)])

  ;; When a `rename` clause, and the new name exists in source (not
  ;; synthesized by e.g. prefix-out) then we'll want to add an
  ;; export-rename-arrow from the new name (export-id) to the old name
  ;; (local-id).
  (unless (equal? local-id export-id)
    (define-values (export-sym export-beg export-end) (stx->vals export-id))
    (define-values (local-sym local-beg local-end) (stx->vals local-id))
    (when (and export-sym export-beg export-end
               local-sym local-beg local-end
               (not (and (= export-beg local-beg)
                         (= export-end local-end))))
      (set-add! (file-pdb-export-renames f)
                (export-rename-arrow phase+space
                                     export-beg
                                     export-end
                                     local-beg
                                     local-end
                                     local-sym
                                     export-sym)))))

(define ((add-import-rename code-str) path mods phase imported-id local-id modpath-stx)
  #;(println (list 'add-import-rename path mods phase old-stx new-stx modpath-stx))
  (define-values (old-sym old-beg old-end) (stx->vals imported-id))
  (define-values (new-sym new-beg new-end) (stx->vals local-id))
  (define-values (_ modpath-beg modpath-end) (stx->vals modpath-stx))
  ;; We're given information about a fully-expanded #%require `rename`
  ;; clause, which results from a surprising variety of things --
  ;; including but definitely _not_ limited to a surface require
  ;; clause like rename-in or only-in where both the old and new names
  ;; are present in the original source. The `rename` clause is a bit
  ;; of a catch-all.
  ;;
  ;; Create an import-rename-arrow only when the imported-id and
  ;; local-id (i.e. old and new names) differ, and, both are present
  ;; in the source (have non-false srcloc, and, the syntax-e matches
  ;; the actual source text).
  (define (in-source? sym beg end)
    (and beg end
         (equal? (substring code-str (sub1 beg) (sub1 end))
                 (~a sym))))
  (define maybe-import-rename-arrow
    (and (not (eq? old-sym new-sym))
         (in-source? old-sym old-beg old-end)
         (in-source? new-sym new-beg new-end)
         (import-rename-arrow phase
                              new-beg new-end
                              old-beg old-end
                              old-sym new-sym)))
  (define maybe-prefix-ranges (syntax-import-or-export-prefix-ranges local-id))
  (hash-set! (file-pdb-import-renames (get path))
             (list modpath-beg modpath-end new-sym)
             (import-rename phase
                            modpath-beg modpath-end
                            maybe-prefix-ranges
                            maybe-import-rename-arrow)))

(define (add-module path mods mod-site sees-enclosing-module-bindings?)
  #;(println (list 'add-module path mods mod-site sees-enclosing-module-bindings?))
  ;; Record for use by completions, for a find-submodule-at-point
  ;; command, and to do semantic highlighting of module names and
  ;; module languages.
  (when mod-site
    (interval-map-set! (file-pdb-modules (get path))
                       (car mod-site) (cdr mod-site)
                       (cons mods sees-enclosing-module-bindings?))))

(define (add-imports path mods syms-or-spec)
  #;(println (list 'add-imports path mods syms-or-specs))
  (hash-update! (file-pdb-imports (get path))
                mods
                (λ (s) (set-union s (if (set? syms-or-spec)
                                        syms-or-spec
                                        (seteq syms-or-spec))))
                (seteq)))

(define (stx->vals stx)
  (define dat (syntax-e stx))
  (define beg (syntax-position stx))
  (define span (syntax-span stx))
  (define end (and beg span (+ beg span)))
  (values dat beg end))
