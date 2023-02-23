#lang racket/base

(require data/interval-map
         drracket/check-syntax
         openssl/sha1
         racket/class
         racket/contract
         racket/dict
         racket/file
         racket/format
         racket/match
         racket/path
         racket/set
         syntax/modread
         "analyze-more.rkt"
         "assert-contract-wrappers.rkt"
         "common.rkt")

(provide open
         close
         analyze-path
         analyze-all-known-paths
         use->def
         nominal-use->def
         rename-sites)

;; Provide some things for exploring interactively in REPL, for e.g.
;; our tests in example.rkt. Not intended to be part of the normal,
;; public API.
(module+ private
  (require data/interval-map)
  (provide files
           get-file
           def->uses/same-name
           (struct-out file)
           (struct-out key)
           (struct-out arrow)
           (struct-out lexical-arrow)
           (struct-out import-arrow)
           (all-from-out data/interval-map)))

;; identifier-binding uniquely refers to a non-lexical binding via a
;; tuple of <path mod phase symbol>. Often we need all but the path
;; since we're working on things per-file. A struct for that:
(struct key (mods phase sym) #:transparent)

;; An arrow always has both ends in the same file. (Arrows for
;; imported definitions point to e.g. the `require` or module language
;; syntax in the importing file. For such arrows, the derived struct
;; `import-arrow`, below, also says where to look outside the file.)
(struct arrow
  (phase
   ;; We don't store `use-beg` or `use-end` here because we assume
   ;; this struct will be the value in an interval-map keyed by those,
   ;; and we don't redundantly store them here.
   use-sym
   def-beg
   def-end
   def-sym)
  #:transparent)

(struct lexical-arrow arrow ())

(struct export-rename-arrow arrow ())

(struct import-rename-arrow arrow ())

;; For syncheck:add-arrow require or module-lang arrows. `from` and
;; `nom` correspond to identifier-binding-resolved fields.
(struct import-arrow arrow
  (from ;(cons path? key?) used to look up in file's `defs` hash-table
   nom  ;(cons path? key?) used to look up in file's `exports hash-table
   ) #:transparent)

(struct file
  (digest            ;(or/c #f string?): sha1
   arrows            ;(interval-map use-beg use-end arrow?)
   defs              ;(hash-table key? (cons def-beg def-end))
   exports           ;(hash-table key? (cons def-beg def-end)
   imports           ;(set key?)
   mouse-overs       ;(interval-map beg end string?)
   tail-arrows       ;(set (cons integer? integer?)
   unused-requires   ;(set (cons beg end)
   sub-range-binders ;(hash-table key? (interval-map ofs-beg ofs-end (list def-beg def-end def-id)
   ) #:transparent)

(define (new-file [digest #f])
  (file digest
        (make-interval-map)
        (make-hash)
        (make-hash)
        (mutable-set)
        (make-interval-map)
        (mutable-set)
        (mutable-set)
        (make-hash)))

(define (open . _)
  (void))

(define (close)
  (void))

(define files (make-hash)) ;path => file

(define (get-file path)
  (match (hash-ref files path #f)
    [(? file? f)
     #:when (file-digest f)
     f]
    [_ (analyze-path path)
       (hash-ref files path)]))

(define sema (make-semaphore 1)) ;coarse guard, for now anyway
(define/contract (analyze-path path
                               #:code    [code #f]
                               #:always? [always? #f])
  (->* (path?)
       (#:code (or/c #f string?)
        #:always? boolean?)
       boolean?)
  (call-with-semaphore
   sema
   (λ ()
     (with-handlers ([exn:fail?
                      (λ (e)
                        (define o (open-output-string))
                        (parameterize ([current-error-port o])
                          ((error-display-handler) (exn-message e) e))
                        (log-pdb-info "error analyzing ~v:\n~a"
                                      path
                                      (get-output-string o))
                        (hash-remove! files path)
                        #f)])
       (define code-str (or code (file->string path #:mode 'text)))
       (define digest (sha1 (open-input-string code-str)))
       (when always?
         (hash-remove! files path))
       (match (hash-ref files path #f)
         [(struct* file ([digest (== digest)])) #f]
         [_ (hash-set! files path (new-file digest))
            (with-time/log (~a "total " path)
              (log-pdb-debug (~a "analyze " path " ..."))
              (analyze-code path code-str)
              #t)])))))

(define (queue-more-files-to-analyze paths)
  (for ([path (in-set paths)])
    (unless (hash-ref files path #f)
      (hash-set! files path (new-file)))))

;; For each known path, this will re-analyze a path when its digest is
;; out-of-date -- or when #:always? is true, in which case all known
;; paths are re-analyzed. Either way, the usual behavior of
;; analyze-path applies: When analyzing a file discovers dependencies
;; on other files, it queues those to be checked for possible
;; (re)analysis, too.
(define (analyze-all-known-paths #:always? [always? #f])
  (define updated-count
    (for/sum ([path (in-hash-keys files)])
      (if (analyze-path path #:always? always?) 1 0)))
  ;; If any analyses ran, re-check in case the analysis added fresh
  ;; files to `digests`.
  (void
   (unless (zero? updated-count)
     (analyze-all-known-paths #:always? #f))))

(define (analyze-code path code-str)
  (assert-drracket-adds-definition-targets-for-contract-wrappers!)
  (string->syntax
   path
   code-str
   (λ (stx)
     (parameterize ([current-namespace (make-base-namespace)])
       (define exp-stx (with-time/log (~a "expand " path) (expand stx)))
       (with-time/log (~a "check-syntax " path)
         (analyze-using-check-syntax path exp-stx code-str))
       (with-time/log (~a "analyze-more " path)
         (analyze-more add-import
                       add-export
                       add-import-rename
                       add-export-rename
                       add-sub-range-binders
                       path
                       exp-stx))))))

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

;;; analyze: using check-syntax

;; Note: drracket/check-syntax reports things as zero-based [from upto)
;; but we handle them as one-based [from upto).

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
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define rb (identifier-binding/resolved src use-stx phase use-sym))
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
                           use-sym
                           phase
                           (add1 def-beg)
                           (add1 def-end)
                           def-sym
                           rb)]
        [else
         (add-lexical-arrow src
                            (add1 use-beg)
                            (add1 use-end)
                            use-sym
                            phase
                            (add1 def-beg)
                            (add1 def-end)
                            def-sym)]))

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
             (key mods phase symbol)
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
                       (key mods phase full-id)
                       (λ (im)
                         (interval-map-set! im
                                            sub-ofs (+ sub-ofs sub-span)
                                            (list def-beg def-end def-id))
                         im)
                       (make-interval-map)))]
      [_ (void)])))

(define (add-import-arrow use-path
                          use-beg use-end use-sym
                          phase
                          def-beg def-end def-sym
                          rb)
  (interval-map-set! (file-arrows (get-file use-path))
                     use-beg
                     (max (add1 use-beg) use-end)
                     (import-arrow phase
                                   use-sym
                                   def-beg
                                   def-end
                                   def-sym
                                   (cons (resolved-binding-from-path rb)
                                         (key (resolved-binding-from-subs rb)
                                              (resolved-binding-from-phase rb)
                                              (resolved-binding-from-sym rb)))
                                   (cons (resolved-binding-nom-path rb)
                                         (key (resolved-binding-nom-subs rb)
                                              (resolved-binding-nom-export-phase rb)
                                              (resolved-binding-nom-sym rb))))))

(define (add-lexical-arrow use-path
                           use-beg use-end use-sym
                           phase
                           def-beg def-end def-sym)
  (interval-map-set! (file-arrows (get-file use-path))
                     use-beg
                     (max (add1 use-beg) use-end)
                     (lexical-arrow phase
                                    use-sym
                                    def-beg
                                    def-end
                                    def-sym)))

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
                                            new-sym
                                            old-beg
                                            old-end
                                            old-sym))))

(define (add-import-rename path subs phase old-stx new-stx path-stx)
  #;(println (list 'add-import-rename path subs old-stx new-stx path-stx))
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
                                            new-sym
                                            old-beg
                                            old-end
                                            old-sym)))
  ;; 2. Update any existing import-arrows pointing to the same
  ;; `modpath` and using new-sym, instead to be lexical arrows
  ;; pointing to `new`. This assumes drracket/check-syntax has already
  ;; run, we are being called from analyze-more, and we need to adjust
  ;; some existing arrows.
  (define-values (path-sym path-beg path-end) (stx->vals path-stx))
  (when (and new-beg new-end path-beg path-end
             (not (= new-beg path-beg))
             (not (= new-end path-end)))
    (define as (file-arrows (get-file path)))
    (for ([(b+e a) (in-dict as)])
      (when (and (import-arrow? a)
                 (equal? (arrow-use-sym a) new-sym)
                 (= (arrow-def-beg a) path-beg)
                 (= (arrow-def-end a) path-end))
        (interval-map-set! as
                           (car b+e)
                           (cdr b+e)
                           (lexical-arrow phase
                                          (arrow-use-sym a)
                                          new-beg
                                          new-end
                                          new-sym)))))
  ;; 3. Also add an import arrow from `old` to `modpath`.
  (when (and old-beg old-end path-beg path-end
             (not (= old-beg path-beg))
             (not (= old-end path-end)))
    (add-import-arrow path
                      old-beg old-end old-sym
                      phase
                      path-beg path-end path-sym
                      (identifier-binding/resolved path old-stx phase
                                                   (syntax->datum old-stx)))))

(define (add-import path subs phase sym)
  (set-add! (file-imports (get-file path))
            (key subs phase sym)))

(define (add-export path subs phase stx)
  #;(println (list 'add-export path subs phase stx))
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (hash-set! (file-exports (get-file path))
                  (key subs phase sym)
                  (cons beg end))]
      [else ;#f beg and/or end
       ;; Assume this is a re-provide arising from all-from,
       ;; all-from-except, or all-from-out. The exported id has no
       ;; srcloc because it does not occur in the source. For the
       ;; graph to work, we need to add this to `exports` and to
       ;; `arrows`. As this isn't actually in the source, we use
       ;; negative unique values for the positions. Things like
       ;; name->uses can filter these. Because `arrows` is an
       ;; interval-map keyed on <use-beg use-end>, we must synthesize
       ;; these values.
       (define smallest
         (for/fold ([smallest 0])
                   ([beg/end (in-dict-keys (file-arrows (get-file path)))])
           (min smallest (car beg/end))))
       (define def-beg (- smallest 4))
       (define def-end (- smallest 3))
       (define use-beg (- smallest 2))
       (define use-end (- smallest 1))
       (hash-set! (file-exports (get-file path))
                  (key subs phase sym)
                  (cons use-beg use-end))
       (add-import-arrow path
                         use-beg use-end sym
                         phase
                         def-beg def-end sym
                         (identifier-binding/resolved path stx phase sym))])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Queries

;; Given a file position, see if it is a use of a definition. If so,
;; return the definition location, else #f. i.e. This is the basis for
;; "find definition".
;;
;; One wrinkle here is that we may already know about a use of a
;; definition in file A, and know it is defined in file B, but we
;; haven't yet analyzed that defining file. We may need to go analyze
;; file B first, so we can find locations within.
;;
;; Another consideration is "how far to jump". When #:nominal? is
;; false, we use the identifier-binding from-xxx fields to find the
;; definition, which elides any arbitrarily long chain of renaming
;; imports and exports (but see use->def wrapper function, below, wrt
;; more jumps for e.g. contracts). Otherwise, when #:nominal? is true,
;; we use the nominal-from-xxx fields, letting arrows trace each such
;; step.
;;
;; Another wrinkle is "sub range binders". Although
;; drracket/check-syntax handles this for lexical arrows, we need to
;; do extra work for import arrows.
(define (use->def* use-path pos #:nominal? nominal? #:same-name? same-name?)
  (match (get-file use-path)
    [(struct* file ([arrows as] [exports exports]))
     (match (interval-map-ref as pos #f)
       [(? lexical-arrow? a)
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        #:when same-name?
        (define-values (beg end _) (interval-map-ref/bounds as pos))
        (list use-path beg end)]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(? import-arrow? a)
        (match-define (cons def-path (key mods phase sym)) (if nominal?
                                                               (import-arrow-nom a)
                                                               (import-arrow-from a)))
        (match (get-file def-path)
          [(struct* file ([digest digest] [defs defs] [exports exports] [sub-range-binders srb]))
           (match (hash-ref (if nominal? exports defs)
                            (key mods phase sym)
                            #f)
             [(cons beg end)
              ;; When the external definition has sub-range-binders, refine
              ;; to where the arrow definition points, based on where within
              ;; the use `pos` is.
              (match (hash-ref srb (key mods phase sym) #f)
                [(? interval-map? srb-im)
                 (define-values (use-beg _use-end _) (interval-map-ref/bounds as pos))
                 (define offset (- pos use-beg))
                 (match (interval-map-ref srb-im offset #f)
                   [(list beg end _) (list def-path beg end)]
                   [_ (list def-path beg end)])]
                [#f (list def-path beg end)])]
             [#f #f])]
          [#f #f])]
       [#f #f])]
    [#f #f]))

;; Like use-pos->def*, but when #:nominal? is false, and when the def
;; loc is also a use of another loc, return that other def.
;;
;; This is to cover cases like contract-out, where identifier-binding
;; will take us to the contract _wrapper_, but we want the "full jump"
;; all the way to the definition wrapped by the contract. So we keep
;; calling use->def* until we arrive at a fixed point.
(define/contract (use->def use-path pos)
  (-> path? exact-positive-integer?
      (or/c #f (list/c path? exact-positive-integer? exact-positive-integer?)))
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
    (match (use->def* use-path pos #:nominal? #f #:same-name? #f)
      [(and v (list def-path def-beg _def-end))
       (if (equal? v previous-answer)
           v
           (loop v def-path def-beg))]
      [#f previous-answer])))

;; The step by step flavor, e.g. useful for full chain of name
;; introductions resulting from imports and exports.
(define/contract (nominal-use->def use-path pos)
  (-> path? exact-positive-integer?
      (or/c #f (list/c path?
                       ;; Not positive; see add-export for all-from-out et al
                       exact-integer?
                       exact-integer?)))
  (use->def* use-path pos #:nominal? #t #:same-name? #f))

;; Find the most distant same-named nominal definition. When pos
;; already is a definition, return its bounds.
(define (use->def/same-name path pos)
  (let loop ([previous-answer #f]
             [path path]
             [pos pos])
    (match (use->def* path pos #:nominal? #t #:same-name? #t)
      [(and v (list def-path def-beg _def-end))
       (if (equal? v previous-answer)
           previous-answer
           (loop v def-path def-beg))]
      [#f previous-answer])))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define (def->uses/same-name def-path pos [result-set (mutable-set)])
  #;(println (list 'def->uses/same-name def-path pos))
  (define (check-exports f path k)
    (define p+k (cons path k))
    (when (hash-has-key? (file-exports f) k)
        #;(printf "~v exports ~v\n" path p+k)
        ;; Go looking for same-named uses of it among all other files
        (for ([(path f) (in-hash files)])
          #;(printf "checking ~v for uses of ~v\n" path p+k)
          (for ([(use-span a) (in-dict (file-arrows f))])
            (when (and (import-arrow? a)
                       (equal? (import-arrow-nom a) p+k)
                       (eq? (arrow-use-sym a) (key-sym k)))
              (match-define (cons use-beg use-end) use-span)
              ;; Ignore re-provides with no actual use sites
              (when (and (positive? use-beg) (positive? use-end))
                #;(printf "2. add result ~v b/c arrow ~v\n" (list path use-beg use-end) a)
                (set-add! result-set (list path use-beg use-end))
                (def->uses/same-name path use-beg result-set))
              (check-exports f path k))))))

  ;; Follow same-named arrows within def-path.
  (define f (get-file def-path))
  (for ([(use-span a) (in-dict (file-arrows f))])
    (match-define (cons use-beg use-end) use-span)
    (define def?
      (cond
        ;; For an import-rename-arrow or export-rename-arrow, we treat
        ;; use-beg..use-end as the "definition", because that is the
        ;; position of the newly introduced name. We don't recur or
        ;; we'd get an endless loop.
        [(and (or (export-rename-arrow? a)
                  (import-rename-arrow? a))
              (<= use-beg pos)
              (< pos use-end))
         #t]
        [(and (<= (arrow-def-beg a) pos)
              (< pos (arrow-def-end a))
              (equal? (arrow-def-sym a) (arrow-use-sym a)))
         (def->uses/same-name def-path use-beg result-set)
         #t]
        [else #f]))
    (when def?
      #;(printf "1. add result ~v b/c arrow ~v\n" (list def-path use-beg use-end) a)
      (set-add! result-set (list def-path use-beg use-end))
      (check-exports f
                     def-path
                     (key '() ;FIXME: syncheck-add-arrow no mods :(
                          (arrow-phase a)
                          (arrow-use-sym a)))))
  result-set)

;; Given a path and position, which may be either a use or a def,
;; return the set of places that must be renamed (the def site as well
;; as all the use sites) if any of them are.
;;
;; Basically we follow the nominal same-name chain from the use to the
;; def (the "root"), then reverse and "fan out" to follow the chains
;; back out through the uses (provided they use the same name). So, we
;; can handle scenarios like something named "foo", then
;; imported/exported as "bar", then imported/exported as "foo". Each
;; of these three is independent wrt renaming; changing the name for
;; each subset has no affect on the others (except maybe at the
;; "ends", e.g. a rename-{in out} where just the old or new name
;; should change).
;;
;; TODO/IDEA: If we return the current/old name, or its beg/end, or
;; its span -- whatever -- then we need only return the beg of each
;; use site. The end will always be name-length positions after beg.
(define (rename-sites path pos)
  ;; Find the def site, which might already be at `pos`.
  (match-define (list def-path def-beg def-end)
    (or (use->def/same-name path pos)
        (for/or ([a (in-dict-values (file-arrows (get-file path)))])
          (and (lexical-arrow? a)
               (<= (arrow-def-beg a) pos)
               (< pos (arrow-def-end a))
               (list path (arrow-def-beg a) (arrow-def-end a))))
        (list #f #f #f)))
  (if (and def-path def-beg def-end)
      (def->uses/same-name def-path def-beg (mutable-set (list def-path def-beg def-end)))
      (mutable-set)))


(module+ ex
  (require racket/runtime-path)
  (define-runtime-path require.rkt "example/require.rkt")
  (define-runtime-path define.rkt "example/define.rkt")
  ;; (use->def require.rkt 42)
  ;; (use->def require.rkt 48)
  ;; (nominal-use->def require.rkt 48)

  ;; (define-runtime-path require-re-provide.rkt "example/require-re-provide.rkt")
  ;; (use->def require-re-provide.rkt 41) ;foo

  ;; (def->uses/same-name define.rkt 88)
  ;; (rename-sites define.rkt 88)
  (analyze-path require.rkt)
  (rename-sites define.rkt 367))
