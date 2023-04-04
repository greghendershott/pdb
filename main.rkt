#lang racket/base

(require data/interval-map
         racket/contract
         racket/match
         racket/set
         "analyze.rkt"
         "data-types.rkt"
         "span-map.rkt"
         (only-in "store.rkt"
                  [get-file store:get-file]
                  read-file-from-sqlite
                  all-known-paths)
         (only-in "nominal-imports.rkt"
                  [lookup files-nominally-importing]))

(provide analyze-path
         all-known-paths
         analyze-all-known-paths
         queue-directory-to-analyze

         get-annotations
         get-completion-candidates
         get-errors
         get-point-info
         get-doc-link

         use->def
         nominal-use->def
         rename-sites)

(define (get-file path)
  (define f (store:get-file path))
  (cond
    [(and (file? f)
          (not (equal? (file-digest f) unknown-digest)))
     f]
    [else
     (analyze-path path)
     (or (store:get-file path)
         (error 'get-file
                "~v\n No analysis available due to an error; see logger topic `pdb`."
                path))]))

;;; Simple queries

;; Most annotations pertain to specific spans. There are various
;; kinds. get-annotations returns most mixed and sorted by position.
;; (See get-errors and get-completion-candidates for two things that
;; get-annotations does /not/ return.)
;;
;; 1. This query supports the sort of access pattern that Racket Mode's
;; "classic" racket-xp-mode uses: Get values for everything and put as
;; text properties into the buffer.
;;
;; That access pattern is not great for large files with a lot of
;; annotation data. It takes space on the front end client (e.g.
;; emacs/vim/vscode). Just as bad is marshaling overhead (converting
;; to/from json or sexp or whatever format is used for the Racket back
;; end to talk to the non-Racket front end).
;;
;; 2. This query also supports getting only a subset for a certain
;; span. This supports better access patterns. See also
;; `get-point-info`, below, which is especially optimized for one such
;; access pattern.
;;
;; TODO: Now that we record zero-span items, notably for things like
;; #%app and #%datum, we should add a flag here to ignore these. Some
;; clients -- certainly Emacs -- can't use these, and they are
;; numerous, so in such cases best not to marshal them at all.
(define/contract (get-annotations path [beg min-position] [end max-position])
  (->* (complete-path?) (position? position?) any) ;returns pdb?
  (define f (get-file path))
  (define (def-sites)
    (for/list ([v (in-list (span-map-refs (arrow-map-def->uses (file-arrows f)) beg end))])
      (match-define (cons (cons def-beg def-end) uses) v)
      (define import? (for/or ([use (in-set uses)]) (import-arrow? use)))
      (list 'def-site
            def-beg
            def-end
            import?
            (sort (for/list ([use (in-set uses)])
                    (list (arrow-use-beg use)
                          (arrow-use-end use)))
                  < #:key car))))
  (define (use-sites)
    (for/list ([v (in-list (span-map-refs (arrow-map-use->def (file-arrows f)) beg end))])
      (match-define (cons (cons use-beg use-end) a) v)
      (list 'use-site
            use-beg
            use-end
            (import-arrow? a)
            (arrow-def-beg a)
            (arrow-def-end a))))
  (define (mouse-overs)
    (for/list ([v (in-list (span-map-refs (file-mouse-overs f) beg end))])
      (match-define (cons (cons beg end) texts) v)
      (list 'mouse-over beg end texts)))
  (define (doc-sites)
    (for/list ([v (in-list (span-map-refs (file-docs f) beg end))])
      (match-define (cons (cons beg end) d) v)
      (list 'doc-link beg end (doc-path d) (doc-anchor d))))
  (define (unused-requires)
    (for/list ([v (in-list (span-map-refs (file-unused-requires f) beg end))])
      (match-define (cons (cons beg end) _) v)
      (list 'unused-require beg end)))
  (sort (append (def-sites)
                (use-sites)
                (mouse-overs)
                (doc-sites)
                (unused-requires))
        < #:key cadr))

;; Optionally accepts a position with the view that someday we'd build
;; a more-targeted data structure for this -- limited to /valid/
;; candidates within a module or even lexical scope. But for now we
;; continue the Racket Mode tradition of erring on the side of
;; offering more candidates, even if some aren't valid.
(define (get-completion-candidates path [_pos min-position])
  (define f (get-file path))
  (set-union
   (for/set ([v (in-set (file-imports f))]) ;-> immutable-set
     v)
   ;; ~= to getting candidates from syncheck:add-definition-target.
   (for/set ([v (in-hash-keys (file-defs f))])
     (ibk-sym v))
   ;; ~= to getting candidates from synchek:add-mouse-over messages
   ;; about "bound occurrence(s)", which includes lexical arrows, plus
   ;; more from our rename-arrows.
   (for*/fold ([s (set)])
              ([uses (in-list (span-map-values (arrow-map-def->uses (file-arrows f))))]
               [use (in-set uses)])
     (match use
       [(? lexical-arrow? a)
        (set-add s (lexical-arrow-sym a))]
       [(? rename-arrow? a)
        (set-add (set-add s (rename-arrow-old-sym a))
                 (rename-arrow-new-sym a))]
       [_ s]))))

;; Accepts no span or position on the theory that, when a file has one
;; or more errors, the user will always want to know and be able to go
;; to all of them, regardless of where they might be in the file.
(define (get-errors path)
  (for/list ([v (in-list (span-map->list (file-errors (get-file path))))])
    (match-define (list (cons beg end) (cons maybe-path message)) v)
    (list beg end
          (or maybe-path (path->string path))
          message)))

;; This is designed for a client that does not want to store any
;; persistent values on its end. For example, an Emacs mode that does
;; not store every annotation as a text property. Instead, upon
;; movement of window-point or window-{start end} (to use Emacs
;; terminology), it can call this to get only values pertaining to
;; that subset of the buffer. Presumably it can temporarily enhance
;; the presentation (e.g. add overlays in Emacs).
;;
;; In principle a client could write this itself by filtering
;; information from `get-annotations` Maybe this shouldn't even exist
;; as part of library, but just be example code? Anyway it's here for
;; now as I dog-food the use of pdb by Racket Mode for Emacs, and
;; learn more from some use in the real world.
(define (get-point-info path pos beg end)
  (define f (get-file path))
  (define (error-messages-here)
    (define-values (beg end a-set) (span-map-ref/bounds (file-errors f) pos #f))
    (and beg end a-set
         (not (set-empty? a-set))
         (list beg end
               (for*/set ([v (in-set a-set)]
                          [err-path (in-value (car v))]
                          [err-msg  (in-value (cdr v))]
                          #:when (or (not err-path)
                                     (equal? err-path (path->string path))))
                 err-msg))))
  ;; TODO: Should we return all mouse-overs for [beg end), in case the
  ;; client wants to support actual GUI tooltips? In that case if the
  ;; client wants to treat a mouse-over at point specially (e.g.
  ;; racket-show in Racket Mode), let it distinguish that itself?
  (define mouse-over
    (or (error-messages-here)
        (call-with-values (λ () (span-map-ref/bounds (file-mouse-overs f) pos #f))
                          (λ (beg end v) (and beg end v (list beg end v))))))
  ;; TODO: Filter use-sites that aren't within [beg end)? In the case
  ;; where there are very many use sites (hundreds or thousands?), it
  ;; could start to matter that we return so many that aren't visible.
  (define-values (def-site use-sites)
    (match (span-map-ref (arrow-map-use->def (file-arrows f)) pos #f)
      [(? arrow? u->d)
       #:when (not (import-arrow? u->d))
       (values (cons (arrow-def-beg u->d)
                     (arrow-def-end u->d))
               (for/list ([d->u (in-set (span-map-ref (arrow-map-def->uses (file-arrows f))
                                                      (arrow-def-beg u->d)
                                                      (set)))])
                 (cons (arrow-use-beg d->u)
                       (arrow-use-end d->u))))]
      [_
       (match (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set))
         [(? set? d->us)
          #:when (not (set-empty? d->us))
          (values (cons (arrow-def-beg (set-first d->us))
                        (arrow-def-end (set-first d->us)))
                  (for/list ([d->u (in-set d->us)]
                             #:when (not (import-arrow? d->u)))
                    (cons (arrow-use-beg d->u)
                          (arrow-use-end d->u))))]
         [_ (values #f #f)])]))
  (define unused-requires
    (map car (span-map-refs (file-unused-requires f) beg end)))
  (define unused-bindings
    (for/list ([v (in-list (span-map-refs (file-mouse-overs f) beg end))]
               #:when (set-member? (cdr v) "no bound occurrences"))
      (car v)))
  (hash
   ;; This pertains only to point
   'mouse-over      mouse-over
   ;; These pertain to point and related sites
   'def-site        def-site
   'use-sites       use-sites
   ;; These pertain to entire beg..end span
   'unused-requires unused-requires
   'unused-bindings unused-bindings))

(define (get-doc-link path pos)
  (span-map-ref (file-docs (get-file path)) pos #f))

(module+ ex
  (require racket/path)
  (get-annotations (simple-form-path "example/define.rkt") 1500 1530)
  (get-annotations (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/require-error.rkt"))
  #;(get-completion-candidates (simple-form-path (build-path "example" "define.rkt")))
  (get-point-info (simple-form-path "example/define.rkt") 1353 1170 1536))

(module+ test
  (require racket/runtime-path
           rackunit)
  (define-runtime-path require.rkt "example/require.rkt")
  (check-true
   (match (for/or ([a (in-list (get-annotations require.rkt 20 21))])
            (and (eq? 'use-site (car a))
                 a))
     [(list 'use-site 20 27 #t 7 18)
      (match (use->def require.rkt 20)
        [(list (? path?) 10485 10492) #t]
        [_ #f])]
     [_ #f])
   "We get a use-site with import? true, for `require`, and, use->def for that site gives the expected location in reqprov.rkt"))

;;; Support for existing client of drracket/check-syntax that don't
;;; care about paging and want syncheck method calls.

(require racket/class
         drracket/check-syntax)

(define/contract (send-to-syncheck-annotations-object path o)
  (-> complete-path? (is-a?/c syncheck-annotations<%>) any)
  (define (find-source-object path)
    (send o
          syncheck:find-source-object
          (datum->syntax #f 'n/a (srcloc path #f #f #f #f))))
  (define path-so (find-source-object path))
  (unless path-so
    (error 'send-to-syncheck-object
           "The find-source-object method of ~v returned false for ~v"
           o
           path))
  (define f (get-file path))
  ;; file-syncheck-arrows => syncheck:add-arrow/name-dup/pxpy
  (for ([v (in-list (span-map->list (file-syncheck-arrows f)))])
    (match-define (cons (cons use-beg use-end) vs) v)
    (for ([v (in-list vs)])
      (match-define (syncheck-arrow def-beg def-end def-px def-py
                                    use-px use-py
                                    actual? phase require-arrow) v)
      (define (name-dup? . _) #f)
      (send o
            syncheck:add-arrow/name-dup/pxpy
            path-so def-beg def-end def-px def-py
            path-so use-beg use-end use-px use-py
            actual? phase require-arrow name-dup?)))
  ;; file-syncheck-jumps => syncheck:add-jump-to-definition/phase-level+space
  (for ([v (in-list (span-map->list (file-syncheck-jumps f)))])
    (match-define (cons (cons beg end) (syncheck-jump sym path mods phase)) v)
    (send o
          syncheck:add-jump-to-definition/phase-level+space
          path-so
          beg
          end
          sym
          path
          mods
          phase))
  ;; file-syncheck-prefix => syncheck:add-prefixed-require-reference
  (for ([v (in-list (span-map->list (file-syncheck-prrs f)))])
    (match-define (cons (cons beg end) (syncheck-prr prefix prefix-beg prefix-end)) v)
    (send o
          syncheck:add-prefixed-require-reference
          path-so
          beg
          end
          prefix
          path-so
          prefix-beg
          prefix-end))
  ;; file-defs => syncheck:add-definition-target/phase-level+space
  (for ([(k v) (in-hash (file-defs f))])
    (match-define (ibk mods phase sym) k)
    (match-define (cons beg end) v)
    (send o
          syncheck:add-definition-target/phase-level+space
          path-so
          (sub1 beg)
          (sub1 end)
          sym
          mods
          phase))
  ;; file-mouse-overs => syncheck:add-mouse-over-status
  (for ([v (in-list (span-map->list (file-mouse-overs f)))])
    (match-define (cons (cons beg end) texts) v)
    (for ([text (in-list texts)])
      (send o
            syncheck:add-mouse-over-status
            path-so
            (sub1 beg)
            (sub1 end)
            text)))
  ;; file-docs => syncheck:add-docs-menu
  (for ([v (in-list (span-map->list (file-docs f)))])
    (match-define (cons (cons beg end) d) v)
    (send o
          syncheck:add-docs-menu
          path-so
          (sub1 beg)
          (sub1 end)
          (doc-sym d)
          (doc-label d)
          (doc-path d)
          (doc-anchor d)
          (doc-anchor-text d)))
  ;; file-require-opens => syncheck:add-require-open-menu
  (for ([v (in-list (span-map->list (file-require-opens f)))])
    (match-define (cons (cons beg end) path) v)
    (send o
          syncheck:add-require-open-menu
          path-so
          (sub1 beg)
          (sub1 end)
          path))
  ;; file-text-types => syncheck:add-text-type
  (for ([v (in-list (span-map->list (file-text-types f)))])
    (match-define (cons (cons beg end) sym) v)
    (send o
          syncheck:add-text-type
          path-so
          (sub1 beg)
          (sub1 end)
          sym))
  ;; file-tail-arrows => syncheck:add-tail-arrow
  (for ([v (in-set (file-tail-arrows f))])
    (match-define (cons tail head) v)
    (send o
          syncheck:add-tail-arrow
          path-so ;?
          (sub1 tail)
          path-so ;?
          (sub1 head))))

(module+ test
  (require data/order
           racket/runtime-path
           (only-in drracket/private/syncheck/traversals
                    build-trace%))
  (define-runtime-path file.rkt "example/require.rkt")
  (analyze-path file.rkt #:always? #t)
  (define o (new build-trace% [src file.rkt]))
  (send-to-syncheck-annotations-object file.rkt o)
  (define (massage xs)
    (define ignored
      '(;; OK to ignore forever
        syncheck:add-id-set
        ;syncheck:add-background-color - seems unused?
        ;syncheck:color-range          - seems unused?

        ;; Tip: You can un-comment one or more of these temporarily,
        ;; when debugging test failures and overhwelmed by huge
        ;; check-equal? output, to help somewhat.
        ;syncheck:add-arrow/name-dup/pxpy
        ;syncheck:add-definition-target/phase-level+space
        ;syncheck:add-docs-menu
        ;syncheck:add-jump-to-definition/phase-level+space
        ;syncheck:add-mouse-over-status
        ;syncheck:add-prefixed-require-reference
        ;syncheck:add-require-open-menu
        ;syncheck:add-tail-arrow
        ;syncheck:add-text-type
        ))
    (for/set ([x (in-list xs)]
              #:when (not (memq (vector-ref x 0) ignored)))
      (case (vector-ref x 0)
        [(syncheck:add-arrow/name-dup/pxpy) ;drop last (name-dup)
         (apply vector (reverse (cdr (reverse (vector->list x)))))]
        [else
         x])))
  (define (->sorted-list a-set)
    (define <? (order-<? datum-order))
    (define (lt? a b)
      (define (cmp-vec v)
        (define is (case (vector-ref v 0)
                     [(syncheck:add-arrow/name-dup/pxpy) '(0 1 2 5 6)]
                     [else                               '(0 1 2)]))
        (for/vector ([i (in-list is)])
          (vector-ref v i)))
      (<? (cmp-vec a) (cmp-vec b)))
    (sort (set->list a-set) lt?))
  (define actual (massage (send o get-trace)))
  (define expected (massage (show-content file.rkt)))
  #;
  (check-equal? (->sorted-list actual)
                (->sorted-list expected)
                "send-to-syncheck-object is equivalent to show-content, modulo order")
  (check-equal? (set-subtract actual expected)
                (set)
                "none unexpected")
  (check-equal? (set-subtract expected actual)
                (set)
                "none missing"))


;;; Queries involving uses and definitions

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
    [(struct* file ([arrows am]))
     (match (span-map-ref (arrow-map-use->def am) pos #f)
       [(? lexical-arrow? a)
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        #:when same-name?
        (list use-path (arrow-use-beg a) (arrow-use-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(? import-arrow? a)
        (match-define (cons def-path def-ibk) (if nominal?
                                                  (import-arrow-nom a)
                                                  (import-arrow-from a)))
        (let loop ([def-path def-path]
                   [def-ibk def-ibk])
          (and (path? def-path) ;not symbol like '#%runtime or '#%core
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
                        (define a (span-map-ref (arrow-map-use->def am) pos #f))
                        (define offset (- pos (arrow-use-beg a)))
                        (match (interval-map-ref srb-im offset #f)
                          [(list beg end _) (list def-path beg end)]
                          [_ (list def-path beg end)])]
                       [#f (list def-path beg end)])]
                    [#f #f])]
                 [#f #f])))]
       [#f #f])]
    [#f #f]))

;; A wrapper for use-pos->def*, using #:nominal? #f. When the def site
;; is a use of another def, return that other def.
;;
;; This is to cover cases like contract-out, where identifier-binding
;; will take us to the contract _wrapper_, but we want the "full jump"
;; all the way to the definition wrapped by the contract. So we keep
;; calling use->def* until we arrive at a fix point.
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

;; A wrapper for use-pos->def*, assuming #:nominal? #t. Single steps
;; through the complete chain of name introductions resulting from
;; imports and exports, including through renames.
(define/contract (nominal-use->def use-path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? exact-integer? exact-integer?)))
  (use->def* use-path pos #:nominal? #t #:same-name? #f))

;; Find the most distant same-named nominal definition.
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
  (or (for/or ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
        (and (lexical-arrow? a)
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; check-syntax might not draw an error to an identifer used in
      ;; a macro definition, as with e.g. `plain-by-macro` or
      ;; `contracted-by-macro` in example/define.rkt. Treat these as
      ;; definition sites anyway.
      (for/or ([beg+end-or-path+ibk (in-hash-values (file-exports f))])
        (match beg+end-or-path+ibk
          [(cons (? position? beg) (? position? end))
           (and (<= beg pos)
                (< pos end)
                (list path beg end))]
          [(cons (? path?) (? ibk?))
           ;; something to do here besides just ignore??
           #f]))
      (for/or ([b+e (in-hash-values (file-defs f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define/contract (def->uses/same-name path beg end)
  (-> complete-path? position? position?
      any #;(hash/c complete-path? (listof (cons/c position? position?))))
  #;(println (list 'def->uses/same-name def-path pos))

  (define (find-uses-in-other-files-of-exports-defined-here f path pos)
    (for ([(ibk def) (in-hash (file-exports f))])
      (match def
        [(cons (? position? def-beg) (? position? def-end))
         (when (and (<= def-beg pos) (< pos def-end))
           (find-uses-of-export (cons path ibk)))]
        [_ (void)])))

  (define (find-uses-of-export path+ibk)
    (for ([path (in-list (files-nominally-importing path+ibk))])
      (define f (read-file-from-sqlite path)) ;get w/o touching cache
      ;; Does this file anonymously re-export the item? If so, go look
      ;; for other files that import it as exported from this file.
      (for ([(export-ibk import-path+ibk) (in-hash (file-exports f))])
        (when (equal? path+ibk import-path+ibk)
          (find-uses-of-export (cons path export-ibk))))
      ;; Check for uses of the export in this file, then see if each
      ;; such use is in turn an export used by other files.
      (for ([a (in-list (arrow-map-arrows (file-arrows f)))])
        (when (and (import-arrow? a)
                   (equal? (import-arrow-nom a) path+ibk))
          (add! path (arrow-use-beg a) (arrow-use-end a))
          (find-uses-in-file path (arrow-use-beg a))
          (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))))))

  (define (find-uses-in-file path pos)
    (define f (get-file path))

    (for ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
      (unless (rename-arrow? a)
        (add! path (arrow-use-beg a) (arrow-use-end a))
        ;; See if use site is an exported definition that is imported
        ;; and used by other analyzed files.
        (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))))

    ;; For a rename-arrow we want to consider the newly introduced
    ;; name -- e.g. the `new` in `(rename-in m [old new])` -- which is
    ;; the "use" position.
    (match (span-map-ref (arrow-map-use->def (file-arrows f)) pos #f)
      [(? rename-arrow? a)
       (add! path (arrow-use-beg a) (arrow-use-end a))
       ;; See if use site is an exported definition that is imported
       ;; and used by other analyzed files.
       (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))]
      [_ (void)])

    ;; See if <path pos> is site is an exported definition that is
    ;; imported and used by other analyzed files. Necessary for e.g.
    ;; all-defined-out.
    (find-uses-in-other-files-of-exports-defined-here f path pos))

  (define ht (make-hash)) ; path? => (set (cons beg end))
  (define (add! path beg end)
    (hash-update! ht
                  path
                  (λ (s) (set-add s (cons beg end)))
                  (set)))

  (add! path beg end)
  (find-uses-in-file path beg)
  (for/hash ([(p s) (in-hash ht)])
    (values p (sort (set->list s) < #:key car))))

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
(define/contract (rename-sites path pos)
  (-> complete-path? position?
      any #;(hash/c complete-path? (listof (cons/c position? position?))))
  ;; Find the def site, which might already be at `pos`.
  (match (or (use->def/same-name path pos)
             (def->def/same-name path pos))
    [(list def-path def-beg def-end)
     (def->uses/same-name def-path def-beg def-end)]
    [#f (make-hash)]))

;; Provide some things for exploring interactively in REPL, for e.g.
;; our tests in example.rkt. Not intended to be part of the normal,
;; public API.
(module+ private
  (require data/interval-map
           "data-types.rkt"
           "span-map.rkt")
  (provide (all-from-out "data-types.rkt")
           (all-from-out "span-map.rkt")
           (all-from-out data/interval-map)
           get-file
           def->def/same-name
           use->def/same-name
           def->uses/same-name))
