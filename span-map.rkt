;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require data/skip-list
         racket/dict
         racket/match
         racket/set
         racket/serialize
         racket/struct)

(provide max-position
         make-span-map
         span-map?
         span-map-set!
         span-map-update*!
         span-map-add!
         span-map-ref/bounds
         span-map-ref
         span-map-refs
         span-map-values
         span-map->list
         span-map-count)

;; Although this is not backed by an interval-map, it is backed by a
;; skip-list with (cons beg end) keys. Unlike an interval-map, it
;; allows storing items where beg=end. span-map-set! does /not/ do any
;; splitting when setting; as a result it is OK for spans to overlap.
;; (But span-map-update*! function will update an existing span with
;; exactly the same beg/end positions.) As a result, the primary
;; lookup functions is span-map-refs, plural. A span-map-ref/bounds
;; wrapper is supplied when a user wants to find for a position just
;; one span with (< beg end).
;;
;; Also this defines prop:serializable.

(module+ test
  (require rackunit))

(define (make-span-map . inits)
  (define sm (span-map (make-skip-list)))
  (for ([init (in-list inits)])
    (match-define (cons (cons beg end) val) init)
    (span-map-set! sm beg end val))
  sm)

(define (skip-list-update! s key updater default)
  (skip-list-set! s key (updater (skip-list-ref s key default))))

(define (span-map-update*! sm beg end updater default)
  (skip-list-update! (span-map-s sm) (cons beg end) updater default))

(define (span-map-set! sm beg end v)
  (span-map-update*! sm beg end (λ _ v) v))

(define (span-map-add! sm beg end v)
  (span-map-update*! sm beg end (λ (vs) (set-add vs v)) (set)))

(define (span-map->list sm)
  (for/list ([(k v) (in-dict (span-map-s sm))])
    (cons k (if (set? v) (set->list v) v))))

(define (span-map-values sm)
  (dict-values (span-map-s sm)))

(define (span-map-count sm)
  (dict-count (span-map-s sm)))

(define (span-map->vector sm)
  (for/vector ([(k v) (in-dict (span-map-s sm))])
    (cons k v)))

(require racket/runtime-path)
(define-runtime-path here ".")
(define span-map-deserialize-info (make-deserialize-info make-span-map void))
(provide span-map-deserialize-info)

(struct span-map (s)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (_) 'make-span-map)
   span-map->list)
  #:property prop:serializable
  (make-serialize-info span-map->vector
                       (cons 'span-map-deserialize-info
                             (module-path-index-join (syntax-source #'here) #f))
                       #f
                       here))

(module+ test
  (let ([sm (make-span-map)])
    (span-map-add! sm 10 20 "foo")
    (span-map-add! sm 10 20 "bar")
    (span-map-add! sm 10 20 "baz")
    (span-map-add! sm 18 19 "not same span")
    (check-equal? (span-map-refs sm 10 20)
                  (list (cons '(10 . 20) (set "foo" "bar" "baz"))
                        (cons '(18 . 19) (set "not same span"))))
    (check-equal? (span-map-ref/bounds sm 15 #f)
                  (cons (cons 10 20) (set "foo" "bar" "baz")))))

(define not-given (gensym 'not-given))

(define (span-map-ref/bounds sm pos [default not-given])
  (define (not-found)
    (cond [(eq? default not-given) (error 'span-map-ref/bounds "not found\n  pos: ~v" pos)]
          [(procedure? default) (default)]
          [else default]))
  (match (span-map-refs sm pos (add1 pos))
    [(list) (not-found)]
    [(list (and v (cons (cons beg end) _val)))
     #:when (< beg end)
     v]
    [(list* many)
     (for/or ([v (in-list many)])
       (match-define (cons (cons beg end) _val) v)
       (and (< beg end) v))]
    [_ (not-found)]))

(define (span-map-ref sm pos #:try-zero-width? [try-zero-width? #f] [default not-given])
  (define (not-found)
    (cond [(eq? default not-given) (error 'span-map-ref "not found\n  pos: ~v" pos)]
          [(procedure? default) (default)]
          [else default]))
  (match (span-map-refs sm pos (add1 pos))
    [(list) (not-found)]
    [(list (cons (cons beg end) val))
     #:when (or try-zero-width? (< beg end))
     val]
    [(list* many)
     (for/or ([v (in-list many)])
       (match-define (cons (cons beg end) val) v)
       (and (or try-zero-width? (< beg end))
            val))]
    [_ (not-found)]))

(define max-position (expt 2 60))
(define (span-map-refs sm from upto)
  (define s (span-map-s sm))
  (let loop ([result null]
             [iter (or (skip-list-iterate-greatest/<=? s (cons from max-position))
                       (skip-list-iterate-least s))])
    (cond
      [iter
       (define key (skip-list-iterate-key s iter))
       (match-define (cons beg end) key)
       (cond
         [(or (<= upto beg)
              (if (= beg end)
                  (< end from) ;treat end as inclusive for beg=end spans
                  (<= end from)))
          (reverse result)]
         [else
          (loop (cons (cons key (skip-list-iterate-value s iter)) result)
                (skip-list-iterate-next s iter))])]
      [else
       (reverse result)])))

#|
      [10   20]
      10-----20
   5-----15
         15-----25
|#

(module+ test
  (define sm (make-span-map))
  (span-map-set! sm 10 20 "10-20")
  (span-map-set! sm 30 40 "30-40")
  (span-map-set! sm 40 45 "40-45")
  (span-map-set! sm 50 50 "50-50")
  (span-map-set! sm 50 60 "50-60")
  (span-map-set! sm 70 70 "70-70")

  (check-equal? (span-map-refs sm 1 10)
                null)
  (check-equal? (span-map-refs sm 5 15)
                '(((10 . 20) . "10-20")))
  (check-equal? (span-map-refs sm 10 20)
                '(((10 . 20) . "10-20")))
  (check-equal? (span-map-refs sm 15 25)
                '(((10 . 20) . "10-20")))
  (check-equal? (span-map-refs sm 25 26)
                null)
  (check-equal? (span-map-refs sm 30 40)
                '(((30 . 40) . "30-40")))
  (check-equal? (span-map-refs sm 1 1000)
                '(((10 . 20) . "10-20")
                  ((30 . 40) . "30-40")
                  ((40 . 45) . "40-45")
                  ((50 . 50) . "50-50")
                  ((50 . 60) . "50-60")
                  ((70 . 70) . "70-70")))
  (check-equal? (span-map-refs sm 100 1000)
                null)

  (check-false (span-map-ref/bounds sm 1 #f))
  (check-equal? (span-map-ref/bounds sm 10 #f)
                '((10 . 20) . "10-20"))
  (check-equal? (span-map-ref/bounds sm 15 #f)
                '((10 . 20) . "10-20"))
  (check-false (span-map-ref/bounds sm 20 #f)
               "end is treated as in half-open interval [beg end)")
  (check-equal? (span-map-ref/bounds sm 40 #f)
                '((40 . 45) . "40-45")
                "end is treated as in half-open interval [beg end)")
  (check-false (span-map-ref/bounds sm 25 #f))
  (check-equal? (span-map-ref/bounds sm 30 #f)
                '((30 . 40) . "30-40"))
  (check-equal? (span-map-ref/bounds sm 50 #f)
                '((50 . 60) . "50-60")
                "span-map-ref/bounds ignores zero-width items")
  (check-equal? (span-map-ref sm 70 #:try-zero-width? #t)
                "70-70"
                "span-map-ref #:try-zero-width? works"))

#;
(define (lookup key)
  (define s (span-map-s sm))
  (define iter (or (skip-list-iterate-greatest/<=? s key)
                   (skip-list-iterate-least s)))
  (and iter (cons (skip-list-iterate-key s iter)
                  (skip-list-iterate-value s iter))))
