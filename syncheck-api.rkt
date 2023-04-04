#lang racket/base

;;; Support for existing client of drracket/check-syntax that don't
;;; care about paging and want syncheck method calls.

(require racket/contract
         racket/class
         racket/match
         racket/set
         drracket/check-syntax
         (only-in "analyze.rkt" get-file)
         "data-types.rkt"
         "span-map.rkt")

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
           (only-in drracket/private/syncheck/traversals
                    build-trace%)
           racket/runtime-path
           rackunit
           (only-in "analyze.rkt"
                    analyze-path))
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
