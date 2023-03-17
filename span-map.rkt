#lang racket/base

(require data/interval-map
         racket/dict
         racket/match
         racket/set)

(provide min-position
         max-position
         make-span-map
         span-map?
         span-map-set!
         span-map-update*!
         span-map-update*!/set
         span-map-ref/bounds
         span-map-ref
         span-map-refs
         span-map-values
         span-map->list)

;; TL;DR: An interval-map with no empty intervals; instead they are
;; represented as intervals with a "no-value" value.
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
(define no-value 'no-value)

(struct span-map (im))

(define (make-span-map . inits)
  (define im (make-interval-map))
  (define sm (span-map im))
  (interval-map-set! im min-position max-position no-value)
  (for ([init (in-list inits)])
    (match-define (cons (cons beg end) val) init)
    (span-map-set! sm beg end val))
  sm)

(define (span-map-set! sm beg end val)
  (interval-map-update*! (span-map-im sm)
                         beg
                         end
                         (λ _ val)
                         (λ () (error 'span-map-set! "should not happen"))))

(define (span-map-update*! sm beg end updater default)
  (interval-map-update*! (span-map-im sm)
                         beg
                         end
                         (λ (v)
                           (cond [(equal? v no-value)
                                  (if (procedure? default) (default) default)]
                                 [else (updater v)]))
                         default))

;; A convenience for how we're mostly going to use this within pdb.
(define (span-map-update*!/set sm beg end v)
  (span-map-update*! sm beg end (λ (s) (set-add s v)) (set v)))

(define not-found (gensym))
(define ((err who))
  (error who "No value found"))

(define (span-map-ref/bounds sm pos [default (err 'span-map-ref/bounds)])
  (define-values (beg end val) (interval-map-ref/bounds (span-map-im sm) pos not-found))
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
         (interval-map-ref/bounds (span-map-im sm) pos))
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
  (for/list ([v (in-dict-values (span-map-im sm))]
             #:when (not (equal? v no-value)))
    v))

(define (span-map->list sm)
  (for/list ([(k v) (in-dict (span-map-im sm))]
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
    (span-map-update*!/set m 10 20 "foo")
    (span-map-update*!/set m 10 20 "bar")
    (span-map-update*!/set m 100 200 "baz")
    (span-map-update*!/set m 150 160 "blah")
    (check-equal? (span-map-refs m 15 16)
                  (list (cons (cons 10 20) (set "foo" "bar"))))
    (check-equal? (span-map->list m)
                  '(((10 . 20) "bar" "foo")
                    ((100 . 150) "baz")
                    ((150 . 160) "baz" "blah")
                    ((160 . 200) "baz"))
                  "span-map->list does not contain any no-value spans, and multiple span-map-update*!/set correctly interleaved values")
    (check-equal? (span-map->list (apply make-span-map (span-map->list m)))
                  (span-map->list m)
                  "round-trip span-map->list through make-span-map")))
