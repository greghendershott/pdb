#lang racket/base

(require data/interval-map
         drracket/check-syntax
         openssl/sha1
         racket/class
         racket/contract
         racket/dict
         racket/file
         racket/format
         racket/logging
         racket/match
         racket/path
         racket/phase+space
         racket/set
         syntax/modread
         "analyze-more.rkt"
         "common.rkt"
         "data-types.rkt"
         "store.rkt")

(provide analyze-path
         analyze-all-known-paths
         queue-directory-to-analyze)

;;; Analysis

;; Analysis used to do a hash-ref in the active files cache, via the
;; same `get-file` function used by query functions. Now, instead, we
;; have the analysis update a fresh `file` struct stored in this
;; parameter.
;;
;; As a sanity check while making the change we also store the path,
;; and compare. (The sanity check shouldn't be necessary because
;; analyze-path guards itself with a semaphore to run only once across
;; all threads, much less only once per-thread. So this is probaby
;; doubly over-defensive.)
(define current-analyzing-file (make-parameter #f)) ;(or/c #f (cons/c path? file?))
(define (get-file path)
  (match (current-analyzing-file)
    [(cons (== path) (? file? f)) f]
    [v (error 'get-file
              "called for ~v but current analyzing file is ~v"
              path v)]))

(define sema (make-semaphore 1)) ;coarse guard, for now anyway
(define/contract (analyze-path path
                               #:code    [code #f]
                               #:always? [always? #f])
  (->* (complete-path?)
       (#:code (or/c #f string?)
        #:always? boolean?)
       boolean?)
  (call-with-semaphore
   sema
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (e)
                        (log-pdb-warning "error analyzing ~v:\n~a" path (exn->string e))
                        (forget-file path)
                        #f)])
       (define code-str (or code (file->string path #:mode 'text)))
       (define digest (sha1 (open-input-string code-str)))
       (when always?
         (forget-file path))
       (match (get-file* path)
         ;; If digest matches, nothing to do
         [(struct* file ([digest (== digest)]))
          #f]
         ;; (Re)analyze.
         [_
          (define f (new-file digest))
          (parameterize ([current-analyzing-file (cons path f)])
            (with-time/log (~a "total " path)
              (log-pdb-debug (~a "analyze " path " ..."))
              (analyze-code path code-str) ;updates the cache, only
              (put-file path f)
              #t))])))))

(define (queue-more-files-to-analyze paths)
  (for ([path (in-set paths)])
    (add-path-if-not-yet-known path (new-file))))

(define/contract (queue-directory-to-analyze path)
  (-> complete-path? any)
  (define (predicate p)
    (equal? #".rkt" (path-get-extension p)))
  (queue-more-files-to-analyze (find-files predicate path)))

;; For each known path, this will re-analyze a path when its digest
;; doesn't match -- or when #:always? is true, in which case all known
;; paths are re-analyzed. Either way, the usual behavior of
;; analyze-path applies: When analyzing a file discovers dependencies
;; on other files, it records those to be checked for possible
;; (re)analysis, too.
(define (analyze-all-known-paths #:always? [always? #f])
  (define updated-count
    (for/sum ([path (in-list (all-known-paths))])
      (if (analyze-path path #:always? always?) 1 0)))
  ;; If any analyses ran, re-check in case any added new files to
  ;; analyze. i.e. run to fix point.
  (void
   (unless (zero? updated-count)
     (analyze-all-known-paths #:always? #f))))

(define/contract (analyze-code path code-str)
  (-> complete-path? string? any)
  (string->syntax
   path
   code-str
   (λ (stx)
     (parameterize ([current-namespace (make-base-namespace)])
       (define exp-stx
         (with-time/log (~a "expand " path)
           (expand/gather-errors-and-mouse-overs stx path code-str)))
       (when exp-stx
         (with-time/log (~a "check-syntax " path)
           (analyze-using-check-syntax path exp-stx code-str))
         (with-time/log (~a "analyze-more " path)
           (analyze-more add-import
                         add-export
                         add-import-rename
                         add-export-rename
                         add-sub-range-binders
                         path
                         exp-stx)))))))

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
   (parameterize ([error-display-handler our-error-display-handler])
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
  (set-add! (file-errors (get-file src-path))
            (list (if (equal? src-path error-path) #f error-path)
                  beg
                  end
                  msg)))

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

;; Note: drracket/check-syntax reports things as zero-based [from
;; upto) but we handle them as one-based [from upto).
(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (field [imported-files (mutable-set)])

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           stx))

    (define/override (syncheck:add-definition-target/phase-level+space
                      _useless beg end sym rev-mods phase)
      (add-def src (add1 beg) (add1 end) (reverse rev-mods) sym phase))

    ;; Note that check-syntax will give us two arrows for prefix-in
    ;; vars.
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      def-stx def-beg def-end _def-px _def-py
                      use-stx use-beg use-end _use-px _use-py
                      _actual? phase require-arrow _name-dup?)
      (when (and (< def-beg def-end)
                 (< use-beg use-end))
        (define def-sym (string->symbol (substring code-str def-beg def-end)))
        (define use-sym (string->symbol (substring code-str use-beg use-end)))
        (define rb (identifier-binding/resolved src use-stx phase))
        (cond
          [(and require-arrow
                ;; Treat use of prefix-in prefix as a lexical-arrow to
                ;; the prefix (instead of an import-arrow to the
                ;; modpath).
                (not
                 (equal? (~a (syntax->datum use-stx))
                         (~a use-sym (resolved-binding-nom-sym rb)))))
           (add-import-arrow src
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
                              use-sym)])))

    (define/override (syncheck:add-require-open-menu _ _beg _end file)
      (set-add! imported-files file))

    (define/override (syncheck:add-mouse-over-status _ beg end str)
      (add-mouse-over-status src (add1 beg) (add1 end) str))

    (define/override (syncheck:add-tail-arrow from-stx from-pos to-stx to-pos)
      (when (and (equal? (syntax-source from-stx) src)
                 (equal? (syntax-source to-stx)   src))
        (add-tail-arrow src (add1 from-pos) (add1 to-pos))))

    (define/override (syncheck:add-unused-require _ beg end)
      (add-unused-require src (add1 beg) (add1 end)))

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
    (queue-more-files-to-analyze (get-field imported-files
                                            (current-annotations)))))

(define (add-def path beg end mods symbol phase)
  #;(println (list 'add-def path beg end mods symbol phase))
  (hash-set! (file-defs (get-file path))
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
         (hash-update! (file-sub-range-binders (get-file path))
                       (ibk mods phase full-id)
                       (λ (im)
                         (interval-map-set! im
                                            sub-ofs (+ sub-ofs sub-span)
                                            (list def-beg def-end def-id))
                         im)
                       (make-interval-map)))]
      [_ (void)])))

(define (add-import-arrow use-path
                          use-beg use-end
                          phase
                          def-beg def-end
                          use-sym
                          rb)
  (interval-map-set! (file-arrows (get-file use-path))
                     use-beg
                     (max (add1 use-beg) use-end)
                     (import-arrow phase
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
  (interval-map-set! (file-arrows (get-file use-path))
                     use-beg
                     (max (add1 use-beg) use-end)
                     (lexical-arrow phase
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
    (interval-map-set! (file-arrows (get-file path))
                       new-beg
                       (max (add1 new-beg) new-end)
                       (export-rename-arrow phase
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
    (interval-map-set! (file-arrows (get-file path))
                       new-beg
                       (max (add1 new-beg) new-end)
                       (import-rename-arrow phase
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
    (define as (file-arrows (get-file path)))
    (for ([(b+e a) (in-dict as)])
      (when (and (import-arrow? a)
                 (equal? (import-arrow-sym a) new-sym)
                 (= (arrow-def-beg a) path-beg)
                 (= (arrow-def-end a) path-end))
        ;; 3. Move original import arrow to point from `old` to
        ;; `modpath`. Important we use original arrow here for its
        ;; import-arrow-from and import-arrow-nom field values.
        (define original-import-arrow (interval-map-ref as (car b+e)))
        (when (and original-import-arrow
                   old-beg old-end path-beg path-end
                   (not (= old-beg path-beg))
                   (not (= old-end path-end)))
          (interval-map-set! as
                             old-beg old-end
                             original-import-arrow))
        (interval-map-set! as
                           (car b+e)
                           (cdr b+e)
                           (lexical-arrow phase
                                          new-beg
                                          new-end
                                          new-sym))))))

(define (add-import path _subs _phase sym)
  #;(println (list 'add-import path _subs _phase sym))
  (set-add! (file-imports (get-file path)) sym))

(define (add-export path subs phase+space stx)
  #;(println (list 'add-export path subs phase+space stx))
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (hash-set! (file-exports (get-file path))
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
          (hash-set! (file-exports (get-file path))
                     (ibk subs phase sym)
                     (cons (resolved-binding-nom-path rb)
                           (ibk (resolved-binding-nom-subs rb)
                                (resolved-binding-nom-export-phase+space rb)
                                (resolved-binding-nom-sym rb))))]
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
  (interval-map-set! (file-mouse-overs (get-file path))
                     beg
                     (max (add1 beg) end)
                     text))

(define (add-tail-arrow path tail head)
  (set-add! (file-tail-arrows (get-file path))
            (cons head tail)))

(define (add-unused-require path beg end)
  (set-add! (file-unused-requires (get-file path))
            (cons beg end)))
