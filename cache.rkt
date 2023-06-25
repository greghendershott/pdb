#lang racket/base

(require racket/match
         syntax/parse/define
         (prefix-in store: "store.rkt"))

(provide get-file
         forget
         put)

;; This acts as a write-through cache for store.rkt. We want things
;; like get-mouse-overs etc. to work fast for the small working set of
;; files the user is editing. (However things like def->uses/same-name
;; may use store.rkt directly to avoid disturbing the working set when
;; they access analysis data for potentially very many files.)

(struct entry (last-access f+d))
(define cache (make-hash)) ;complete-path? => entry?
(define current-cache-maximum-entries (make-parameter 32))
(define sema (make-semaphore 1))
(define-simple-macro (with-semaphore e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

(define (now) (current-inexact-monotonic-milliseconds))

(define (get-file path [desired-digest #f])
  (with-semaphore
    (match (hash-ref cache path #f)
      [(entry _time (and f+d (store:file+digest file digest)))
       #:when (or (not desired-digest)
                  (equal? desired-digest digest))
       ;; cache hit, but update the last-access time
       (hash-set! cache path (entry (now) f+d))
       file]
      [_ ;cache miss
       (match (store:get-file+digest path desired-digest)
         [(and f+d (store:file+digest file _digest))
          (hash-set! cache path (entry (now) f+d))
          (maybe-remove-oldest!) ;in case cache grew
          file]
         [#f #f])])))

(define (forget path)
  (with-semaphore
    (hash-remove! cache path)
    (store:forget path)))

(define (put path file digest #:exports exports #:re-exports re-exports #:imports imports)
  (with-semaphore
    (hash-set! cache path (entry (now) (store:file+digest file digest)))
    (maybe-remove-oldest!)
    (store:put path file digest #:exports exports #:re-exports re-exports #:imports imports)))

(define (maybe-remove-oldest!)
  ;; assumes called in with-semaphore from get-file or put
  (when (>= (hash-count cache) (current-cache-maximum-entries))
    (define-values (oldest-path _)
      (for/fold ([oldest-path #f]
                 [oldest-time +inf.0])
                ([(path entry) (in-hash cache)])
        (if (< (entry-last-access entry) oldest-time)
            (values path        (entry-last-access entry))
            (values oldest-path oldest-time))))
    (hash-remove! cache oldest-path)))
