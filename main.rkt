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
         "common.rkt")

(provide load
         save
         analyze-code ;works for non-file code, e.g. for IDE
         analyze-path
         analyze-all-known-paths
         queue-directory-to-analyze
         use->def
         nominal-use->def
         rename-sites)

;;; Data types

(define position? exact-positive-integer?)

;; identifier-binding uniquely refers to a non-lexical binding via a
;; tuple of <path mod phase symbol>. Often we need all but the path
;; since we're working on things per-file. A struct for that:
(struct ibk (mods phase sym) #:prefab)

;; An arrow always has both ends in the same file. (Arrows for
;; imported definitions point to e.g. the `require` or module language
;; syntax in the importing file. For such arrows, the derived struct
;; `import-arrow`, below, also says where to look outside the file.)
(struct arrow
  (phase
   ;; We don't store `use-beg` or `use-end` here because we assume
   ;; this struct will be the value in an interval-map keyed by those,
   ;; and we don't redundantly store them here.
   def-beg
   def-end)
  #:prefab)

(struct lexical-arrow arrow (sym) #:prefab)

(struct rename-arrow arrow (old-sym new-sym) #:prefab)
(struct export-rename-arrow rename-arrow () #:prefab)
(struct import-rename-arrow rename-arrow () #:prefab)

;; For syncheck:add-arrow require or module-lang arrows. `from` and
;; `nom` correspond to identifier-binding-resolved fields.
(struct import-arrow arrow
  (use-sym
   mod-sym
   from ;(cons path? key?) used to look up in file's `defs` hash-table
   nom  ;(cons path? key?) used to look up in file's `exports hash-table
   ) #:prefab)

(define (arrow-use-sym a)
  (cond [(lexical-arrow? a) (lexical-arrow-sym a)]
        [(rename-arrow? a)  (rename-arrow-new-sym a)]
        [(import-arrow? a)  (import-arrow-use-sym a)]))

(define (arrow-def-sym a)
  (cond [(lexical-arrow? a) (lexical-arrow-sym a)]
        [(rename-arrow? a)  (rename-arrow-old-sym a)]
        [(import-arrow? a)  (import-arrow-mod-sym a)]))

;; When changing fields here, also update `new-file` and the
;; `file-massage-xxx` functions, just below.
(struct file
  (digest            ;(or/c #f string?): sha1
   arrows            ;(interval-map use-beg use-end arrow?)
   defs              ;(hash-table ibk? (cons def-beg def-end))
   exports           ;(hash-table ibk? (or/c (cons def-beg def-end) (cons path? ibk?))
   imports           ;(set symbol?)
   mouse-overs       ;(interval-map beg end string?)
   tail-arrows       ;(set (cons integer? integer?)
   unused-requires   ;(set (cons beg end)
   sub-range-binders ;(hash-table key? (interval-map ofs-beg ofs-end (list def-beg def-end def-id)
   ) #:prefab)

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

;; Massage data to/from the subset that racket/serialize requires.
;; Includes details like making sure that on load we have mutable
;; hash-tables when the default might be immutable.
(define (file-massage-before-serialize f)
  (struct-copy
   file f
   [arrows            (dict->list (file-arrows f))]
   [imports           (set->list (file-imports f))]
   [tail-arrows       (set->list (file-tail-arrows f))]
   [unused-requires   (set->list (file-unused-requires f))]
   [mouse-overs       (dict->list (file-mouse-overs f))]
   [sub-range-binders (for/hash ([(k v) (in-hash (file-sub-range-binders f))])
                        (values k (dict->list v)))]))

(define (file-massage-after-deserialize f)
  (struct-copy
   file f
   [arrows            (make-interval-map (file-arrows f))]
   [imports           (apply mutable-set (file-imports f))]
   [tail-arrows       (apply mutable-set (file-tail-arrows f))]
   [unused-requires   (apply mutable-set (file-unused-requires f))]
   [mouse-overs       (make-interval-map (file-mouse-overs f))]
   [sub-range-binders (make-hash ;mutable
                       (for/list ([(k v) (in-hash (file-sub-range-binders f))])
                         (cons k (make-interval-map v))))]))

(define files (make-hash)) ;complete-path? => file?

;;; Analysis

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
  (->* (complete-path?)
       (#:code (or/c #f string?)
        #:always? boolean?)
       boolean?)
  (call-with-semaphore
   sema
   (λ ()
     (with-handlers (#;
                     [exn:fail?
                      (λ (e)
                        (define o (open-output-string))
                        (parameterize ([current-error-port o])
                          ((error-display-handler) (exn-message e) e))
                        (log-pdb-warning "error analyzing ~v:\n~a"
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
    (for/sum ([path (in-hash-keys files)])
      (if (analyze-path path #:always? always?) 1 0)))
  ;; If any analyses ran, re-check in case the analysis added new
  ;; files, i.e. run to fix point.
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
                             phase
                             (add1 def-beg)
                             (add1 def-end)
                             use-sym
                             def-sym
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
                          mod-sym
                          rb)
  (interval-map-set! (file-arrows (get-file use-path))
                     use-beg
                     (max (add1 use-beg) use-end)
                     (import-arrow phase
                                   def-beg
                                   def-end
                                   use-sym
                                   mod-sym
                                   (cons (resolved-binding-from-path rb)
                                         (ibk (resolved-binding-from-subs rb)
                                              (resolved-binding-from-phase rb)
                                              (resolved-binding-from-sym rb)))
                                   (cons (resolved-binding-nom-path rb)
                                         (ibk (resolved-binding-nom-subs rb)
                                              (resolved-binding-nom-export-phase rb)
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
                 (equal? (arrow-use-sym a) new-sym)
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
  #;(println (list 'add-import path subs phase sym))
  (set-add! (file-imports (get-file path)) sym))

(define (add-export path subs phase stx)
  #;(println (list 'add-export path subs phase stx))
  (define-values (sym beg end) (stx->vals stx))
  (when sym
    (cond
      [(and beg end)
       (hash-set! (file-exports (get-file path))
                  (ibk subs phase sym)
                  (cons beg end))]
      [else
       ;; The exported id has no srcloc because it does not occur in
       ;; the source (e.g. all-from, all-from-except, or
       ;; all-from-out).
       (define rb (identifier-binding/resolved path stx phase sym))
       (hash-set! (file-exports (get-file path))
                  (ibk subs phase sym)
                  (cons (resolved-binding-nom-path rb)
                        (ibk (resolved-binding-nom-subs rb)
                             (resolved-binding-nom-export-phase rb)
                             (resolved-binding-nom-sym rb))))])))

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
    [(struct* file ([arrows as]))
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
        (match-define (cons def-path def-ibk) (if nominal?
                                                  (import-arrow-nom a)
                                                  (import-arrow-from a)))
        (let loop ([def-path def-path]
                   [def-ibk def-ibk])
          (match (get-file def-path)
            [(struct* file ([defs defs] [exports exports] [sub-range-binders srbs]))
             (match (hash-ref (if nominal? exports defs) def-ibk #f)
               [(cons (? path? p) (? ibk? i)) ;follow re-provide
                (if (and (equal? p def-path) (equal? i def-ibk))
                    #f
                    (loop p i))]
               [(cons (? position? beg) (? position? end))
                ;; When the external definition has sub-range-binders, refine
                ;; to where the arrow definition points, based on where within
                ;; the use `pos` is.
                (match (hash-ref srbs def-ibk #f)
                  [(? interval-map? srb-im)
                   (define-values (use-beg _use-end _) (interval-map-ref/bounds as pos))
                   (define offset (- pos use-beg))
                   (match (interval-map-ref srb-im offset #f)
                     [(list beg end _) (list def-path beg end)]
                     [_ (list def-path beg end)])]
                  [#f (list def-path beg end)])]
               [#f #f])]
            [#f #f]))]
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
  (-> complete-path? position?
      (or/c #f (list/c complete-path? position? position?)))
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
    (match (use->def* use-path pos #:nominal? #f #:same-name? #f)
      [(and this-answer (list def-path def-beg _def-end))
       (if (equal? this-answer previous-answer)
           this-answer
           (loop this-answer def-path def-beg))]
      [#f previous-answer])))

;; The step by step flavor, e.g. useful for full chain of name
;; introductions resulting from imports and exports.
(define/contract (nominal-use->def use-path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? exact-integer? exact-integer?)))
  (use->def* use-path pos #:nominal? #t #:same-name? #f))

;; Find the most distant same-named nominal definition. When pos
;; already is a definition, return its bounds.
(define/contract (use->def/same-name path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? position? position?)))
  (let loop ([previous-answer #f]
             [path path]
             [pos pos])
    (match (use->def* path pos #:nominal? #t #:same-name? #t)
      [(and this-answer (list def-path def-beg def-end))
       ;; Handle the hacky negative "positions" by uing them as a
       ;; stepping stone to the next point, but ignoring them as a
       ;; possible final answer to be returned.
       (if (equal? this-answer previous-answer)
           this-answer
           (loop (if (and (position? def-beg) (position? def-end))
                     this-answer
                     previous-answer)
                 def-path
                 def-beg))]
      [#f previous-answer])))

;; Is <path pos> a definition site?
;;
;; Used by `rename-sites` to handle the case where it is given a def
;; site as opposed to a use site -- so if use->def/same-name fails,
;; try this.
(define (def->def/same-name path pos)
  (define f (get-file path))
  (or (for/or ([a (in-dict-values (file-arrows f))])
        (and (lexical-arrow? a)
             (<= (arrow-def-beg a) pos)
             (< pos (arrow-def-end a))
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; check-syntax might not draw an error to an identifer used in
      ;; a macro definition, as with e.g. `plain-by-macro` or
      ;; `contracted-by-macro` in example/define.rkt. Treat these as
      ;; definition sites anyway.
      (for/or ([b+e (in-hash-values (file-exports f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))
      (for/or ([b+e (in-hash-values (file-defs f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define/contract (def->uses/same-name path pos [result-set (mutable-set)])
  (->* (complete-path? position?)
       ((set/c (list/c complete-path? position? position?) #:kind 'mutable))
       (set/c (list/c complete-path? position? position?) #:kind 'mutable))
  #;(println (list 'def->uses/same-name def-path pos))

  (define (find-uses-in-other-files-of-exports-defined-here f path pos)
    (for ([(ibk def) (in-hash (file-exports f))])
      (match def
        [(cons (? position? def-beg) (? position? def-end))
         (when (and (<= def-beg pos) (< pos def-end))
           (find-uses-of-export (cons path ibk)))]
        [_ (void)])))

  (define (find-uses-of-export path+ibk)
    (for* ([(path f) (in-hash files)])
      ;; Does this file anonymously re-export the item? If so, go look
      ;; for other files that import it as exported from this file.
      (for ([(export-ibk import-path+ibk) (in-hash (file-exports f))])
        (when (equal? path+ibk import-path+ibk)
          (find-uses-of-export (cons path export-ibk))))
      ;; Check for uses of the export in this file, then see if each
      ;; such use is in turn an export used by other files.
      (for ([(use-span a) (in-dict (file-arrows f))])
        (when (and (import-arrow? a)
                   (equal? (import-arrow-nom a) path+ibk))
          (match-define (cons use-beg use-end) use-span)
          (set-add! result-set (list path use-beg use-end))
          (find-uses-in-file path use-beg)
          (find-uses-in-other-files-of-exports-defined-here f path use-beg)))))

  (define (find-uses-in-file path pos)
    (define f (get-file path))
    (for ([(use-span a) (in-dict (file-arrows f))])
      (match-define (cons use-beg use-end) use-span)
      (when (if (rename-arrow? a)
                ;; For a rename-arrow we want to consider the newly
                ;; introduced name -- e.g. the `new` in `(rename-in m
                ;; [old new])` -- which is the "use" position.
                (and (<= use-beg pos) (< pos use-end))
                ;; Otherwise condider the "def" position, provided the
                ;; def and use syms are the same.
                (and (<= (arrow-def-beg a) pos) (< pos (arrow-def-end a))
                     (equal? (arrow-def-sym a) (arrow-use-sym a))))
        (set-add! result-set (list path use-beg use-end))
        ;; See if use site is an exported definition that is imported
        ;; and used by other analyzed files.
        (find-uses-in-other-files-of-exports-defined-here f path use-beg)))
    ;; See if <path pos> is site is an exported definition that is
    ;; imported and used by other analyzed files. Necessary for e.g.
    ;; all-defined-out.
    (find-uses-in-other-files-of-exports-defined-here f path pos))

  (find-uses-in-file path pos)
  result-set)

;; Given a path and position, which may be either a use or a def,
;; return the set of places that must be renamed (the def site as well
;; as all the use sites) if any of them are.
;;
;; Basically we follow the nominal same-name chain from the use to the
;; def (the "root"), then reverse and "fan out" to follow the tree
;; back out through all uses sharing a name. So, we can handle
;; scenarios like something named "foo", then imported/exported as
;; "bar", then imported/exported as "foo". Each of these three is
;; independent wrt renaming; changing the name for each subset has no
;; affect on the others (except maybe at the "ends", e.g. a rename-{in
;; out} where just the old or new name should change).
;;
;; TODO/IDEA: If we return the current/old name, or its beg/end, or
;; its span -- whatever -- then we need only return the beg of each
;; use site. The end will always be name-length positions after beg.
;;
;; TODO/IDEA: Could return (hash/c path? (setof position?)).
;; Presumably the tool will find it more useful to change one file at
;; a time.
(define/contract (rename-sites path pos)
  (-> complete-path? position?
      (set/c (list/c complete-path? position? position?) #:kind 'mutable))
  ;; Find the def site, which might already be at `pos`.
  (match-define (list def-path def-beg def-end)
    (or (use->def/same-name path pos)
        (def->def/same-name path pos)
        (list #f #f #f)))
  (if (and def-path def-beg def-end)
      (def->uses/same-name def-path def-beg (mutable-set (list def-path def-beg def-end)))
      (mutable-set)))

;; Provide some things for exploring interactively in REPL, for e.g.
;; our tests in example.rkt. Not intended to be part of the normal,
;; public API.
(module+ private
  (require data/interval-map)
  (provide files
           get-file
           def->def/same-name
           use->def/same-name
           def->uses/same-name
           (struct-out file)
           (struct-out ibk)
           (struct-out arrow)
           (struct-out lexical-arrow)
           (struct-out import-arrow)
           arrow-def-sym
           arrow-use-sym
           arrow-use
           arrow-def
           (all-from-out data/interval-map))
  (define (arrow-use path pos)
    (interval-map-ref/bounds (file-arrows (get-file path)) pos))
  (define (arrow-def path pos)
    (for*/list ([(b+e a) (in-dict (file-arrows (get-file path)))]
                #:when (and (<= (arrow-def-beg a) pos)
                            (< pos (arrow-def-end a))))
      (list b+e a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Persistence

;; For now, just serialize everything in one .rktd.gz file.
;;
;; Using gzip helps a lot, especially with e.g. the highly repetitive
;; mouse-overs strings.
;;
;; [Instead: Could imagine writing similar serialized data as a blob
;; to a sqlite table, one row per file; perhaps with the
;; fully-expanded syntax for each file in another column. In this
;; case, our `files` hash-table could be a weak hash-table used more
;; like a write-through cache.]

(require racket/serialize
         file/gzip
         file/gunzip)

(define (write-data out)
  (displayln ";; zero or more (cons path file) mappings" out)
  (for ([(p f) (in-hash files)])
    (writeln
     (serialize
      (cons (path->string p)
            (file-massage-before-serialize f)))
     out)))

(define (read-data in)
  ;; zero or more (cons path files) mappings
  (hash-clear! files)
  (let loop ()
    (define v (read in))
    (unless (eof-object? v)
      (match-define (cons (? string? p) (? file? f)) (deserialize v))
      (hash-set! files
                 (string->path p)
                 (file-massage-after-deserialize f))
      (loop))))

(define (write-data/gzip fout)
  (define-values (pin pout) (make-pipe))
  (thread (λ ()
            (write-data pout)
            (close-output-port pout)))
  (gzip-through-ports pin fout #f 0))

(define (read-data/gunzip fin)
  (define-values (pin pout) (make-pipe))
  (thread (λ ()
            (read-data pin)
            (close-output-port pout)))
  (gunzip-through-ports fin pout))

(define (save path)
  (with-time/log (~v `(save ,path))
    (call-with-output-file* #:mode 'text #:exists 'replace
      path write-data/gzip)))

(define (load path)
  (with-time/log (~v `(load ,path))
    (with-handlers ([exn:fail? (λ _ #f)])
      (call-with-input-file* #:mode 'text
        path read-data/gunzip)
      #t)))

(module+ test
  (require rackunit)
  (hash-clear! files)
  (analyze-path (path->complete-path (build-path "example" "define.rkt")))
  (save "test.rktd.gz")
  (define original files)
  (hash-clear! files)
  (load "test.rktd.gz")
  (check-equal? original files))
