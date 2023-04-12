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
         racket/phase+space
         syntax/modread
         "analyze-more.rkt"
         "common.rkt"
         "data-types.rkt"
         (prefix-in store: "store.rkt"))

(provide get-file
         analyze-path
         analyzing-threads-count
         add-directory
         forget-path
         forget-directory)

(define/contract (get-file path)
  (-> complete-path? file?)
  (match (store:get-file path)
    [(? file? f) f]
    [#f
     (define ch (make-channel))
     (spawn-do-analyze-path path
                            #:code          #f
                            #:always?       #f
                            #:import-depth  0
                            #:response-chan ch)
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
;; imported files takes place on other threads, and does not
;; meaningfully delay returning results for the original file.
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
                         #:code          code
                         #:always?       always?
                         #:import-depth  import-depth
                         #:response-chan ch)
  (match (sync ch)
    [(? exn:fail? e)  (raise e)]
    [(? exn:break? e) 'abandoned]
    [(? file?)        'completed]))

;;; Worker threads

;; For now this creates unlimited threads, as opposed to being
;; something like a limited job queue for threads or even places. I
;; think this might actually be fine for typical access patterns by
;; end user tools --- with the possible exception of being called via
;; add-directory for hundreds or thousands of files.
(define sema (make-semaphore 1)) ;guard concurrent use of ht
(define ht   (make-hash))        ;path? => thread?

(define (spawn-do-analyze-path path
                               #:code          [code #f]
                               #:always?       [always? #f]
                               #:import-depth  [import-depth 0]
                               #:response-chan [response-chan #f])
  (define (do-analyze-thunk)
    (match-define (do-analyze-path-result file-or-exn imports)
      (do-analyze-path path code always?))
    (when response-chan (channel-put response-chan file-or-exn))
    (call-with-semaphore sema (λ () (hash-remove! ht path)))
    (when (< 0 import-depth)
      (define new-depth (sub1 import-depth))
      (unless (set-empty? imports)
        (log-pdb-debug "about to spawn threads for ~v imports of ~v:"
                       (set-count imports)
                       path))
      (for ([path (in-set imports)])
        (spawn-do-analyze-path path #:import-depth new-depth))))
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
     (hash-set! ht path (thread do-analyze-thunk)))))

(define (analyzing-threads-count)
  (call-with-semaphore sema (λ () (hash-count ht))))

;; Analyze all files in and under `dir`. The optional #:import-depth
;; and #:always? args are the same as for `analyze-path`.
(define/contract (add-directory dir
                                #:import-depth [import-depth 0]
                                #:always?      [always? #f])
  (->* (complete-path?)
       (#:import-depth exact-nonnegative-integer?
        #:always?      boolean?)
       any)
  (for ([path (in-directory dir)])
    (when (equal? (path-get-extension path) #".rkt")
      (spawn-do-analyze-path (simple-form-path path)
                             #:import-depth import-depth
                             #:always?      always?))))

(define (forget-paths paths)
  (for ([path (in-set paths)])
    (store:forget (simple-form-path path))))

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
              path v)]))

;; Collect things for which we'll make one call to add-nominal-imports
;; when done.
(define current-nominal-imports (make-parameter #f)) ;(or/c #f (set/c (con/c path ibk)))
(define (gather-nominal-import rb)
  (when (path? (resolved-binding-nom-path rb)) ;as opposed to e.g. '#%core
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
(struct do-analyze-path-result (file-or-exn imports))
(define (do-analyze-path path code always?)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-pdb-warning "error analyzing ~v:\n~a" path (exn->string e))
                     (forget-path path)
                     (do-analyze-path-result e null))]
                  [exn:break?
                   (λ (e)
                     (log-pdb-debug "got exn:break for ~v" path)
                     (do-analyze-path-result e null))])
    (define code-str (or code (file->string path #:mode 'text)))
    (define digest (sha1 (open-input-string code-str)))
    (define orig-f (store:get-file path))
    (cond
      [(or always?
           (not orig-f)
           (not (equal? (file-digest orig-f) digest)))
       (define f (new-file digest))
       (parameterize ([current-analyzing-file (cons path f)]
                      [current-nominal-imports (mutable-set)])
         (with-time/log (~a "total " path)
           (log-pdb-info (~a "analyze " path " ..."))
           (define imports (analyze-code path code-str))
           (with-time/log "update db"
             (store:put path f (current-nominal-imports)))
           (do-analyze-path-result f imports)))]
      [else
       (do-analyze-path-result orig-f null)])))

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
         [current-directory-for-user (find-system-path 'pref-dir)])
      (with-handlers ([exn:fail? handle-fail])
        (expand stx))))

  ;; 2. There exists a protocol for macros to communicate tooltips to
  ;; DrRacket via a log-message to the logger 'online-check-syntax.
  ;; Alhtough this might seem strange, the motivation is that e.g. a
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
                   (? exact-positive-integer? beg)
                   (? exact-positive-integer? end)
                   (or (? string? string-or-thunk)
                       (? procedure? string-or-thunk)))
           (when (equal? src-path (syntax-source stx))
             ;; Force now; the resulting string will likely use less
             ;; memory than a thunk closure.
             (define (force v) (if (procedure? v) (v) v))
             (define str (force string-or-thunk))
             (add-mouse-over-status src-path (add1 beg) (add1 end) str))]
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
  (span-map-add! (file-errors (get src-path))
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

;; Note: drracket/check-syntax reports things as zero-based [from
;; upto) but we handle them as one-based [from upto).
(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (field [imported-files (mutable-set)])

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           (wrapper stx)))

    (define/override (syncheck:add-definition-target/phase-level+space
                      _so beg end sym rev-mods phase)
      (add-def src (add1 beg) (add1 end) (reverse rev-mods) sym phase))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                      _so beg end sym path mods phase)
      (add-syncheck-jump src beg end sym path mods phase))

    (define/override (syncheck:add-prefixed-require-reference
                      _req-so req-pos-left req-pos-right prefix
                      _prefix-so prefix-left prefix-right)
      (add-prefix-require-reference src
                                    req-pos-left req-pos-right prefix
                                    prefix-left prefix-right))

    ;; Note that check-syntax will give us two arrows for prefix-in
    ;; vars.
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      def-so def-beg def-end def-px def-py
                      use-so use-beg use-end use-px use-py
                      actual? phase require-arrow _name-dup?)
      (add-syncheck-arrow src
                          def-beg def-end def-px def-py
                          use-beg use-end use-px use-py
                          actual? phase require-arrow)
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define def-stx (wrapper-stx def-so))
      (define use-stx (wrapper-stx use-so))
      (define rb (identifier-binding/resolved src use-stx phase))
      (cond
        [(and require-arrow
              ;; Treat use of prefix-in prefix as a lexical-arrow to
              ;; the prefix (instead of an import-arrow to the
              ;; modpath). FIXME: This test is very ad hoc. Looks for
              ;; name mismatch, but not zero-width items like #%app or
              ;; #%datum.
              (not
               (and (equal? (~a (syntax->datum use-stx))
                            (~a use-sym (resolved-binding-nom-sym rb)))
                    (< use-beg use-end))))
         (add-import-arrow (eq? require-arrow 'module-lang)
                           src
                           (add1 use-beg)
                           (add1 use-end)
                           phase
                           (add1 def-beg)
                           (add1 def-end)
                           use-sym
                           rb)]
        [else
         (unless (equal? use-sym def-sym)
           (log-pdb-warning "lexical arrow with differing use and def syms ~v"
                            (list use-stx use-sym def-stx def-sym)))
         (add-lexical-arrow src
                            (add1 use-beg)
                            (add1 use-end)
                            phase
                            (add1 def-beg)
                            (add1 def-end)
                            use-sym)]))

    (define/override (syncheck:add-require-open-menu _so beg end filename)
      (set-add! imported-files filename)
      (add-require-open src (add1 beg) (add1 end) filename))

    (define/override (syncheck:add-mouse-over-status _so beg end str)
      (add-mouse-over-status src (add1 beg) (add1 end) str))

    (define/override (syncheck:add-tail-arrow _from-so from-pos _to-so to-pos)
      ;; Our find-source-object insists on both matching analyzed source.
      (unless (= from-pos to-pos)
        (add-tail-arrow src
                        (add1 from-pos)
                        (add1 to-pos))))

    (define/override (syncheck:add-docs-menu _so beg end sym label path anchor anchor-text)
      (add-docs src (add1 beg) (add1 end) sym label path anchor anchor-text))

    (define/override (syncheck:add-unused-require _so beg end)
      (add-unused-require src (add1 beg) (add1 end)))

    (define/override (syncheck:add-text-type _so beg end type)
      (add-text-type src (add1 beg) (add1 end) type))

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

(define (add-def path beg end mods symbol phase)
  #;(println (list 'add-def path beg end mods symbol phase))
  (hash-set! (file-defs (get path))
             (ibk mods phase symbol)
             (cons beg end)))

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
         (hash-update! (file-sub-range-binders (get path))
                       (ibk mods phase full-id)
                       (λ (im)
                         (interval-map-set! im
                                            sub-ofs (+ sub-ofs sub-span)
                                            (list def-beg def-end def-id))
                         im)
                       (make-interval-map)))]
      [_ (void)])))

(define (add-import-arrow module-lang?
                          use-path
                          use-beg use-end
                          phase
                          def-beg def-end
                          use-sym
                          rb)
  (gather-nominal-import rb)
  (arrow-map-set! (file-arrows (get use-path))
                  ((if module-lang? lang-import-arrow import-arrow)
                   phase
                   use-beg
                   use-end
                   def-beg
                   def-end
                   use-sym
                   (cons (resolved-binding-from-path rb)
                         (ibk (resolved-binding-from-subs rb)
                              (resolved-binding-from-phase rb)
                              (resolved-binding-from-sym rb)))
                   (cons (resolved-binding-nom-path rb)
                         (ibk (resolved-binding-nom-subs rb)
                              (resolved-binding-nom-export-phase+space rb)
                              (resolved-binding-nom-sym rb))))))

(define (add-lexical-arrow use-path
                           use-beg
                           use-end
                           phase
                           def-beg
                           def-end
                           sym)
  (arrow-map-set! (file-arrows (get use-path))
                  (lexical-arrow phase
                                 use-beg
                                 use-end
                                 def-beg
                                 def-end
                                 sym)))

(define (add-export-rename path subs phase old-stx new-stx)
  #;(println (list 'add-export-rename path subs old-stx new-stx))
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and old-beg old-end new-beg new-end
             (not (= old-beg new-beg))
             (not (= old-end new-end)))
    (arrow-map-set! (file-arrows (get path))
                    (export-rename-arrow phase
                                         new-beg
                                         new-end
                                         old-beg
                                         old-end
                                         old-sym
                                         new-sym))))

(define (add-import-rename path subs phase old-stx new-stx path-stx)
  #;(println (list 'add-import-rename path subs phase old-stx new-stx path-stx))
  ;; Given
  ;;
  ;;     (require (rename-in modpath [old new]))
  ;;     new
  ;;
  ;; or same with `only-in`:
  ;;
  ;; 1. Add import-rename-arrow from new to old.
  (define-values (old-sym old-beg old-end) (stx->vals old-stx))
  (define-values (new-sym new-beg new-end) (stx->vals new-stx))
  (when (and new-beg new-end
             (not (equal? old-beg new-beg))
             (not (equal? old-end new-end)))
    (arrow-map-set! (file-arrows (get path))
                    (import-rename-arrow phase
                                         new-beg
                                         new-end
                                         old-beg
                                         old-end
                                         old-sym
                                         new-sym)))
  ;; 2. Update any existing import-arrows pointing to the same
  ;; `modpath` and using new-sym, instead to be lexical arrows
  ;; pointing to `new`. This assumes drracket/check-syntax has already
  ;; run, we are being called from analyze-more, and we need to adjust
  ;; some existing arrows.
  (define-values (_path-sym path-beg path-end) (stx->vals path-stx))
  (when (and new-beg new-end path-beg path-end
             (not (= new-beg path-beg))
             (not (= new-end path-end)))
    (define am (file-arrows (get path)))
    (match-define (arrow-map def->uses use->def) am)
    (for ([a (in-set (span-map-ref def->uses path-beg (set)))])
      (when (and (import-arrow? a)
                 (equal? (import-arrow-sym a) new-sym))
        ;; 3. Move original import arrow to point from `old` to
        ;; `modpath`. Important we use original arrow here for its
        ;; import-arrow-from and import-arrow-nom field values.
        (define original-import-arrow (span-map-ref use->def (arrow-use-beg a)))
        (when (and original-import-arrow
                   old-beg old-end path-beg path-end
                   (not (= old-beg path-beg))
                   (not (= old-end path-end)))
          (arrow-map-set! am
                          (import-arrow (arrow-phase original-import-arrow)
                                        old-beg
                                        old-end
                                        (arrow-def-beg original-import-arrow)
                                        (arrow-def-end original-import-arrow)
                                        (import-arrow-sym original-import-arrow)
                                        (import-arrow-from original-import-arrow)
                                        (import-arrow-nom original-import-arrow))))
        (arrow-map-set! am
                        (lexical-arrow phase
                                       (arrow-use-beg a)
                                       (arrow-use-end a)
                                       new-beg
                                       new-end
                                       new-sym))))))

(define (add-import path _subs _phase sym)
  #;(println (list 'add-import path _subs _phase sym))
  (set-add! (file-imports (get path)) sym))

(define (add-export path subs phase+space stx)
  #;(println (list 'add-export path subs phase+space stx))
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (hash-set! (file-exports (get path))
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
          (hash-set! (file-exports (get path))
                     (ibk subs phase sym)
                     (cons (resolved-binding-nom-path rb)
                           (ibk (resolved-binding-nom-subs rb)
                                (resolved-binding-nom-export-phase+space rb)
                                (resolved-binding-nom-sym rb))))
          (gather-nominal-import rb)]
         [#f
          (log-pdb-warning "~v was #f"
                           `(identifier-binding/resolved ,path ,stx ,phase))])])))

(define (stx->vals stx)
  (define dat (syntax-e stx))
  (define beg (syntax-position stx))
  (define span (syntax-span stx))
  (define end (and beg span (+ beg span)))
  (values dat beg end))

(define (add-mouse-over-status path beg end text)
  ;; Note: There may exist multiple mouse-over messages for the same
  ;; interval, such as both "imported from X" and "N binding
  ;; occurrences".
  (span-map-add! (file-mouse-overs (get path))
                 beg
                 end
                 text))

(define (add-tail-arrow path head-pos tail-pos)
  (set-add! (file-tail-arrows (get path))
            (cons head-pos
                  tail-pos)))

(define (add-docs path beg end sym label doc-path-string anchor anchor-text)
  (span-map-set! (file-docs (get path))
                 beg
                 end
                 (doc sym label doc-path-string anchor anchor-text)))

(define (add-unused-require path beg end)
  (span-map-set! (file-unused-requires (get path))
                 beg
                 end
                 #t))

(define (add-require-open path beg end req-path)
  (span-map-set! (file-require-opens (get path))
                 beg
                 end
                 req-path))

(define (add-text-type path beg end type)
  (span-map-set! (file-text-types (get path))
                 beg
                 end
                 type))

(define (add-syncheck-arrow path
                            def-beg def-end def-px def-py
                            use-beg use-end use-px use-py
                            actual? phase require-arrow)
  (span-map-add! (file-syncheck-arrows (get path))
                 use-beg
                 use-end
                 (syncheck-arrow def-beg def-end def-px def-py
                                 use-px use-py
                                 actual? phase require-arrow)))

(define (add-syncheck-jump path beg end sym jump-path mods phase)
  (span-map-set! (file-syncheck-jumps (get path))
                 beg
                 end
                 (syncheck-jump sym jump-path mods phase)))

(define (add-prefix-require-reference path beg end prefix prefix-beg prefix-end)
  (span-map-set! (file-syncheck-prrs (get path))
                 beg end
                 (syncheck-prr prefix prefix-beg prefix-end)))
