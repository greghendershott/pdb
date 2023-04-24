;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require drracket/check-syntax
         openssl/sha1
         racket/class
         racket/contract
         racket/file
         racket/format
         racket/logging
         racket/match
         racket/path
         (only-in racket/port open-output-nowhere)
         racket/phase+space
         syntax/modread
         "analyze-more.rkt"
         "common.rkt"
         "data-types.rkt"
         (prefix-in cache: "cache.rkt")
         (prefix-in store: (only-in "store.rkt" get-digest)))

(provide get-file
         analyze-path
         add-directory
         forget-path
         forget-directory)

(define/contract (get-file path)
  (-> complete-path? file?)
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
       [(? exn? e)  (raise e)]
       [(? file? f) f])]))

;; Intended for an editor tool to call whenever the user has made
;; changes.
;;
;; #:code allows analyzing changes not saved to a file on disk.
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
(define/contract (analyze-path path
                               #:code         [code #f]
                               #:import-depth [import-depth 0]
                               #:always?      [always? #f])
  (->* (complete-path?)
       (#:code         (or/c #f string?)
        #:import-depth exact-nonnegative-integer?
        #:always?      boolean?)
       any) ;(or/c 'abandoned 'completed)
  (define ch (make-channel))
  (spawn-do-analyze-path path
                         #:code         code
                         #:always?      always?
                         #:import-depth import-depth
                         #:result-chan  ch)
  (match (sync ch)
    [(? exn:fail? e)  (raise e)]
    [(? exn:break? e) 'abandoned]
    [(? file?)        'completed]))

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
        #:result-chan  [result-chan #f]      ;to block for file? or exn? result
        #:imports-chan [imports-chan #f]     ;to block until all imports done
        #:done-paths   [done (mutable-set)]) ;to avoid even sha1 compares
      (define (do-analyze-thunk)
        (set-add! done path)
        (match-define (cons file-or-exn imports) (do-analyze-path path code always? 0))
        (when result-chan (channel-put result-chan file-or-exn))
        (let do-imports ([depth 1] [imports imports])
          (when (<= depth max-depth)
            (for ([path (in-set imports)])
              (unless (set-member? done path)
                (set-add! done path)
                (match-define (cons _ imports) (do-analyze-path path #f #f depth))
                (do-imports (add1 depth) imports)))))
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
  (->* (complete-path?)
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
  (-> complete-path? any)
  (forget-paths (list path)))

(define/contract (forget-directory path)
  (-> complete-path? any)
  (forget-paths (find-files values path)))

;;; Analysis per se

(define current-analyzing-file (make-parameter #f)) ;(or/c #f (cons/c path? file?))
(define (get path)
  (match (current-analyzing-file)
    [(cons (== path) (? file? f)) f]
    [v (error 'get
              "called for ~v but current analyzing file is ~v"
              path (car v))]))

;; Collect things for which we'll make one call to add-nominal-imports
;; when done.
(define current-nominal-imports (make-parameter #f)) ;(or/c #f (set/c (con/c path ibk)))
(define (gather-nominal-import rb)
  (when (path? (resolved-binding-nom-path rb)) ;as opposed to e.g. '#%core
    (unless (current-nominal-imports)
      (error 'gather-nominal-import "called with current-nominal-imports false"))
    (set-add! (current-nominal-imports)
               (cons (resolved-binding-nom-path rb)
                     (ibk (resolved-binding-nom-subs rb)
                          (resolved-binding-nom-export-phase+space rb)
                          (resolved-binding-nom-sym rb))))))

;; analyze-path returns false if the file has already been analyzed
;; and hasn't changed.
;;
;; #:code may a string with the source code (e.g. from a live edit
;; buffer); when false the contents of path are used as the code.
;;
;; #:always? forces an (re)analysis; mainly useful during development
;; and testing for this library.
(define (do-analyze-path path code-str always? depth)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-pdb-warning "error analyzing ~v:\n~a" path (exn->string e))
                     (forget-path path)
                     (cons e null))]
                  [exn:break?
                   (λ (e)
                     (log-pdb-debug "got exn:break for ~v" path)
                     (cons e null))])
    (define code (or code-str (file->string path #:mode 'text)))
    (define digest (sha1 (open-input-string code)))
    (define orig-f (cache:get-file path digest))
    (cond
      [(or always?
           (not orig-f))
       (define f (make-file))
       (parameterize ([current-analyzing-file (cons path f)]
                      [current-nominal-imports (mutable-set)])
         (define pre (build-string depth (λ _ #\space)))
         (with-time/log (~a pre "total " path)
           (log-pdb-info (~a pre "analyze " path " ..."))
           (define imports (analyze-code path code))
           (with-time/log "add our arrows"
             (file-add-arrows f))
           (with-time/log "update db"
             (cache:put path f digest (current-nominal-imports)))
           (cons f imports)))]
      [else
       (cons orig-f null)])))

(define (analyze-code path code-str)
  ;; (-> complete-path? string? (set/c path?))
  (define dir (path-only path))
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-load-relative-directory dir]
                 [current-directory               dir])
    (define stx (with-module-reading-parameterization
                  (λ ()
                    (define in (open-input-string code-str path))
                    (port-count-lines! in)
                    (match (read-syntax path in)
                      [(? eof-object?) #'""]
                      [(? syntax? stx) stx]))))
    (parameterize ([current-namespace (make-base-namespace)])
       (define exp-stx
         (with-time/log (~a "expand " path)
           (expand/gather-errors-and-mouse-overs stx path code-str)))
       (cond
         [exp-stx
          (define import-paths
            (with-time/log (~a "check-syntax " path)
              (analyze-using-check-syntax path exp-stx code-str)))
          (with-time/log (~a "analyze-more " path)
            (analyze-more add-import
                          add-export
                          add-import-rename
                          add-export-rename
                          add-sub-range-binders
                          path
                          exp-stx))
          import-paths]
         [else
          (set)]))))

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
                            (+ pos span) msg)]
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
                     (add1 req-pos-left)
                     (add1 req-pos-right)
                     (syncheck-prefixed-require-reference
                      prefix (add1 prefix-left) (add1 prefix-right))))

    (define/override (syncheck:add-arrow/name-dup/pxpy
                      def-so def-beg def-end def-px def-py
                      use-so use-beg use-end use-px use-py
                      actual? phase require-arrow _name-dup?)
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define use-stx (wrapper-stx use-so))
      (define rb (identifier-binding/resolved src use-stx phase))
      (when require-arrow
        (gather-nominal-import rb))
      (set-add! (file-syncheck-arrows (get src))
                (syncheck-arrow (add1 def-beg) (add1 def-end) def-px def-py
                                (add1 use-beg) (add1 use-end) use-px use-py
                                actual? phase require-arrow
                                (syntax->datum use-stx) use-sym def-sym rb)))

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

(define (add-sub-range-binders mods phase srb)
  #;(println (list 'add-sub-range-binders mods phase srb))
  (let loop ([srb srb])
    (match srb
      [(cons this more)
       (loop this)
       (loop more)]
      [(or (vector full-stx sub-ofs sub-span
                   def-stx def-ofs def-span)
           (vector full-stx sub-ofs sub-span _ _
                   def-stx def-ofs def-span _ _))
       (define path (syntax-source def-stx))
       (when (and path
                  ;; TODO: Not sure how to handle when different.
                  ;; Arises with e.g.
                  ;; <pkgs>/redex-benchmark/redex/benchmark/models/rvm/rvm-14.rkt.
                  (equal? path (car (current-analyzing-file)))
                  (path-string? path)
                  (syntax-position full-stx)
                  (syntax-position def-stx))
         (define full-id (syntax-e full-stx))
         (define def-id  (string->symbol
                          (substring (symbol->string full-id)
                                     sub-ofs
                                     (+ sub-ofs sub-span))))
         (define def-beg (+ (syntax-position def-stx) def-ofs))
         (define def-end (+ def-beg def-span))
         (hash-update! (file-pdb-sub-range-binders (get path))
                       (ibk mods phase full-id)
                       (λ (im)
                         (interval-map-set! im
                                            sub-ofs (+ sub-ofs sub-span)
                                            (list def-beg def-end def-id))
                         im)
                       (make-interval-map)))]
      [_ (void)])))

(define (add-export-rename path subs phase old-stx new-stx)
  #;(println (list 'add-export-rename path subs old-stx new-stx))
  ;; This entails just adding one new arrow, so go ahead and store
  ;; an export-rename-arrow in the set, to add later.
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and old-beg old-end new-beg new-end
             (not (= old-beg new-beg))
             (not (= old-end new-end)))
    (set-add! (file-pdb-export-renames (get path))
              (export-rename-arrow phase
                                   new-beg
                                   new-end
                                   old-beg
                                   old-end
                                   old-sym
                                   new-sym))))

(define (add-import-rename path subs phase old-stx new-stx modpath-stx)
  #;(println (list 'add-import-rename path subs phase old-stx new-stx path-stx))
  ;; Because this involves both adding new arrows as well as changing
  ;; some existing arrows, we simply record all the info here to do
  ;; the work later.
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (define-values (_ modpath-beg modpath-end) (stx->vals modpath-stx))
  (set-add! (file-pdb-import-renames (get path))
            (list phase
                  old-sym old-beg old-end
                  new-sym new-beg new-end
                  modpath-beg modpath-end)))

(define (add-import path _subs _phase sym)
  #;(println (list 'add-import path _subs _phase sym))
  (set-add! (file-pdb-imports (get path)) sym))

(define (add-export path subs phase+space stx)
  #;(println (list 'add-export path subs phase+space stx))
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (hash-set! (file-pdb-exports (get path))
                  (ibk subs phase+space sym)
                  (cons beg end))]
      [else
       ;; The exported id has no srcloc because it does not occur in
       ;; the source (e.g. all-from, all-from-except, or
       ;; all-from-out). Create an `exports` item where the definition
       ;; is a path+ibk.
       ;;
       ;; NOTE: identifier-binding errors if given phase+space; wants
       ;; just phase.
       (define phase (phase+space-phase phase+space))
       (match (identifier-binding/resolved path stx phase)
         [(? resolved-binding? rb)
          (hash-set! (file-pdb-exports (get path))
                     (ibk subs phase sym)
                     (cons (resolved-binding-nom-path rb)
                           (ibk (resolved-binding-nom-subs rb)
                                (resolved-binding-nom-export-phase+space rb)
                                (resolved-binding-nom-sym rb))))
          (gather-nominal-import rb)]
         [#f
          (log-pdb-warning "could not add export because false:\n  ~v"
                           `(identifier-binding/resolved ,path ,stx ,phase))])])))

(define (stx->vals stx)
  (define dat (syntax-e stx))
  (define beg (syntax-position stx))
  (define span (syntax-span stx))
  (define end (and beg span (+ beg span)))
  (values dat beg end))
