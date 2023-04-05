;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match)

(provide min-position
         max-position
         make-span-map
         span-map?
         span-map-set!
         span-map-update*!
         span-map-add!
         span-map-ref/bounds
         span-map-ref
         span-map-refs
         span-map-values
         span-map->list)

;; An interval-map uses half-open intervals [beg end), with the
;; invariant (< beg end).
;;
;; Furthermore, our analysis for arrows -- for purposes of
;; definitions, uses, and rename-sites -- only cares about and works
;; for non-zero-width spans of the end user source code. (We can't
;; very well rename a portion of the source that is... zero characters
;; wide.)
;;
;; But now we want to store some (= beg end) items produced by
;; drracket/check-syntax for things like #%app and #%datum. The
;; motivation is to provide an interface for clients who want us to
;; call syncheck methods, instead of them using things like our
;; `get-annotations` API. Those clients may need/want the zero-width
;; items.
;;
;; I spent some time trying to make a new span-map, backed by a
;; skip-list, that would mostly preserve interval-map semantics while
;; also cramming in the zero-width (i.e. non-interval) items. Although
;; I think I got close, I also got very confused. I couldn't use that
;; to accomodate the zero-width items, and also pass tests like those
;; in example.rkt.
;;
;; ---
;;
;; So next I'm trying the approach of "sharding". Use two maps, one
;; for non-zero items as before, and another for zero items. (The
;; latter are stored as (= end (add1 beg)) so we can keep using the
;; backing interval-map, and adjusted when retrieved; we could instead
;; some non-interval reprsentation, but for now it's simpler to reuse
;; span-map.)
;;
;; Certain functions like span-map-ref ignore the zero map, because I
;; know they're used only by my analysis, and because zero-width items
;; destroy the idea that a ref for a position has no more than one
;; item. Imagine asking about position 10 when there exists both [10
;; 10] and [10 20). What is something like use->def supposed to with
;; multiple answers.
;;
;; Other functions like span-map-refs look in both maps and append the
;; results.
;;
;; This file moves the original span-map code into a submodule,
;; defines the sharding two-map wrapper, and exports the wrapper using
;; the original names.

(module inner racket/base

  (require data/interval-map
           racket/dict
           racket/match
           racket/set)

  (provide min-position
           max-position
           make-span-map
           span-map-set!
           span-map-update*!
           span-map-add!
           span-map-ref/bounds
           span-map-ref
           span-map-refs
           span-map-values
           span-map->list)

  ;; TL;DR: An interval-map with no gaps; instead they are represented
  ;; as intervals with a "no-value" value.
  ;;
  ;; An interval-map is perfect for looking up a single value given a
  ;; position. Having found that, however, there is no way to iterate
  ;; from that point forward up to another position. In other words, you
  ;; can lookup a single span, but not a span of spans (not without
  ;; iterating from the start of the dict). Unfortunately looking up a
  ;; span of spans is an access pattern we want to support: e.g., "Give
  ;; me all mouse-over annotations in the span [beg end)."
  ;;
  ;; Probably there is some bespoke tree data structure I should create
  ;; -- and if I do so someday, the provided functions here should
  ;; remain a suitable interface. But for now, I'm going to use a "hack"
  ;; where we make an interval-map with an initial huge span having the
  ;; value "no-value". Then our span-map-set! actually uses
  ;; interval-map-update*! to carve a span out of that, which does have
  ;; values. Likewise our span-map-update*! is aware of the no-value
  ;; sections.
  ;;
  ;; As a result, the initial no-value "background" span continues to
  ;; exist between spans that have other values. So an
  ;; interval-map-ref/bounds will always get _some_ result, it will
  ;; never return #f. Either it gets a value, or it gets no-value.
  ;; Either way, the interval end bound says where to check next. So
  ;; that's how we can continue iterating from the initial lookup point.

  (define min-position 1)
  (define max-position (expt 2 31))
  (define no-value (gensym 'no-value))

  (struct -span-map (im))

  (define (make-span-map . inits)
    (define im (make-interval-map))
    (define sm (-span-map im))
    (interval-map-set! im min-position max-position no-value)
    (for ([init (in-list inits)])
      (match-define (cons (cons beg end) val) init)
      (span-map-set! sm beg end val))
    sm)

  (define (span-map-set! sm beg end val)
    (interval-map-update*! (-span-map-im sm)
                           beg
                           end
                           (λ _ val)
                           (λ () (error 'span-map-set! "should not happen"))))

  (define (span-map-update*! sm beg end updater default)
    (interval-map-update*! (-span-map-im sm)
                           beg
                           end
                           (λ (v)
                             (cond [(equal? v no-value)
                                    (if (procedure? default) (default) default)]
                                   [else (updater v)]))
                           default))

  ;; A convenience for how we're mostly going to use this within pdb.
  (define (span-map-add! sm beg end v)
    (span-map-update*! sm beg end (λ (s) (set-add s v)) (set v)))

  (define not-found (gensym 'not-found))
  (define ((err who))
    (error who "No value found"))

  (define (span-map-ref/bounds sm pos [default (err 'span-map-ref/bounds)])
    (define-values (beg end val) (interval-map-ref/bounds (-span-map-im sm) pos not-found))
    (match val
      [(== not-found)(error 'span-map-ref/bounds "should not happen")]
      [(== no-value) (values #f #f (if (procedure? default) (default) default))]
      [v             (values beg end v)]))

  (define (span-map-ref sm pos [default (err 'span-map-ref)])
    (define-values (_beg _end val) (span-map-ref/bounds sm pos default))
    val)

  ;; Return all the spans in [overall-beg overall-end)
  (define (span-map-refs sm overall-beg overall-end)
    (let loop ([pos     overall-beg]
               [results null])
      (cond
        [(< pos overall-end)
         (define-values (beg end val)
           (interval-map-ref/bounds (-span-map-im sm) pos))
         (cond
           [(equal? val no-value)
            (loop end
                  results)]
           [else
            (define result (cons (cons beg end) val))
            (loop end
                  (cons result results))])]
        [else (reverse results)])))

  (define (span-map-values sm)
    (for/list ([v (in-dict-values (-span-map-im sm))]
               #:when (not (equal? v no-value)))
      v))

  (define (span-map->list sm)
    (for/list ([(k v) (in-dict (-span-map-im sm))]
               #:when (not (equal? v no-value)))
      (cons k (if (set? v)
                  (set->list v)
                  v))))

  (module+ test
    (require rackunit)
    (let ([m (make-span-map)])
      (span-map-set! m 10 20 "foo")
      (span-map-set! m 30 40 "bar")
      (span-map-set! m 50 60 "baz")
      (span-map-set! m 70 80 "blah")
      (check-equal? (span-map-refs m 35 55)
                    '(((30 . 40) . "bar")
                      ((50 . 60) . "baz")))
      (check-equal? (span-map->list m)
                    '(((10 . 20) . "foo")
                      ((30 . 40) . "bar")
                      ((50 . 60) . "baz")
                      ((70 . 80) . "blah"))
                    "span-map->list does not contain any no-value spans")
      (check-equal? (span-map->list (apply make-span-map (span-map->list m)))
                    (span-map->list m)
                    "round-trip span-map->list through make-span-map"))
    (let ([m (make-span-map)])
      (span-map-add! m 10 20 "foo")
      (span-map-add! m 10 20 "bar")
      (span-map-add! m 100 200 "baz")
      (span-map-add! m 150 160 "blah")
      (check-equal? (span-map-refs m 15 16)
                    (list (cons (cons 10 20) (set "foo" "bar"))))
      (check-equal? (span-map->list m)
                    '(((10 . 20) "bar" "foo")
                      ((100 . 150) "baz")
                      ((150 . 160) "baz" "blah")
                      ((160 . 200) "baz"))
                    "span-map->list does not contain any no-value spans, and multiple span-map-add! correctly interleaved values")
      (check-equal? (span-map->list (apply make-span-map (span-map->list m)))
                    (span-map->list m)
                    "round-trip span-map->list through make-span-map"))))

(require racket/match
         (prefix-in inner: 'inner)
         (only-in 'inner
                  min-position
                  max-position))

(struct span-map (non-zero zero))

(define (make-span-map . inits)
  (define sm (span-map (inner:make-span-map)
                       (inner:make-span-map)))
  (for ([init (in-list inits)])
    (match-define (cons (cons beg end) val) init)
    (span-map-set! sm beg end val))
  sm)

(define (call-with-inner-map-and-positions who sm beg end proc)
  (unless (span-map? sm) (error who "not an outer span-map:\n  sm: ~e" sm))
  (cond [(< beg end) (proc (span-map-non-zero sm) beg       end)]
        [(= beg end) (proc (span-map-zero     sm) beg (add1 end))]
        [else (error who "(> beg end)")]))

(define (span-map-set! sm beg end val)
  (call-with-inner-map-and-positions
   'span-map-set! sm beg end
   (λ (ism beg end)
     (inner:span-map-set! ism beg end val))))

(define (span-map-update*! sm beg end updater default)
  (call-with-inner-map-and-positions
   'span-map-update*! sm beg end
   (λ (ism beg end)
     (inner:span-map-update*! ism beg end updater default))))

(define (span-map-add! sm beg end v)
  (call-with-inner-map-and-positions
   'span-map-add! sm beg end
   (λ (ism beg end)
     (inner:span-map-add! ism beg end v))))

(define ((err who))
  (error who "No value found"))

;; ONLY considers non-zero-width items
(define (span-map-ref/bounds sm pos [default (err 'span-map-ref/bounds)])
  (inner:span-map-ref/bounds (span-map-non-zero sm) pos default))

;; ONLY considers non-zero-width items
(define (span-map-ref sm pos [default (err 'span-map-ref)])
  (inner:span-map-ref (span-map-non-zero sm) pos default))

;; Returns both, appended, adjusting the zero-width items' end back to
;; (= beg end).
(define (span-map-refs sm overall-beg overall-end)
  (define non-zero-width-items
    (inner:span-map-refs (span-map-non-zero sm) overall-beg overall-end))
  (define zero-width-items
    (for/list ([v (in-list (inner:span-map-refs (span-map-zero sm)
                                                overall-beg overall-end))])
      (match-define (cons (cons beg _end) val) v)
      (cons (cons beg beg) val)))
  (append non-zero-width-items
          zero-width-items))

;; Returns both, appended.
(define (span-map-values sm)
  (append (inner:span-map-values (span-map-non-zero sm))
          (inner:span-map-values (span-map-zero sm))))

;; Returns both, appended, adjusting the zero-width items' end back to
;; (= beg end).
(define (span-map->list sm)
  (append (inner:span-map->list (span-map-non-zero sm))
          (for/list ([v (in-list (inner:span-map->list (span-map-zero sm)))])
            (match-define (cons (cons beg _end) val) v)
            (cons (cons beg beg) val))))

(module+ test
  (require rackunit)
  (define m (make-span-map))
  (span-map-set! m 1 2 "1 2")
  (span-map-set! m 2 3 "2 3")
  (span-map-set! m 1 1 "1 1")
  (span-map-set! m 2 2 "2 2")
  (check-equal? (span-map-ref m 1)
                "1 2")
  (check-equal? (span-map-ref m 2)
                "2 3")
  (check-equal? (span-map-refs m 1 2)
                '(((1 . 2) . "1 2")
                  ((1 . 1) . "1 1")))
  (check-equal? (span-map-values m)
                '("1 2" "2 3" "1 1" "2 2"))
  (check-equal? (span-map->list m)
                '(((1 . 2) . "1 2")
                  ((2 . 3) . "2 3")
                  ((1 . 1) . "1 1")
                  ((2 . 2) . "2 2"))))
