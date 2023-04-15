#lang racket/base

(require racket/match
         syntax/parse/define
         (prefix-in store: "store.rkt"))

(provide get-file
         forget
         put)

;; This acts as a write-through cache for store.rkt. We want things
;; like analyze-path and get-mouse-overs etc. to work fast for the
;; small working set of files the user is editing. (However things
;; like def->uses/same-name may use store.rkt directly to bypass the
;; cache, thereby preserving the working set.)

(struct entry (time f+d))
(define cache (make-hash)) ;complete-path? => entry?
(define current-cache-maximum-entries (make-parameter 32))
(define sema (make-semaphore 1))
(define-simple-macro (with-semaphore e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

(define (get-file path [desired-digest #f])
  (with-semaphore
    (match (hash-ref cache path #f)
      [(entry _time (and f+d (store:file+digest file digest)))
       #:when (or (not desired-digest)
                  (equal? desired-digest digest))
       ;; cache hit, but update the last-access time
       (hash-set! cache path (entry (current-seconds) f+d))
       file]
      [_ ;cache miss
       (match (store:get-file+digest path desired-digest)
         [(and f+d (store:file+digest file _digest))
          (hash-set! cache path (entry (current-seconds) f+d))
          (maybe-remove-oldest!) ;in case cache grew
          file]
         [#f #f])])))

(define (forget path)
  (with-semaphore
    (hash-remove! cache path)
    (store:forget path)))

(define (put path file digest exports-used)
  (with-semaphore
    (hash-set! cache path
               (entry (current-seconds) (store:file+digest file digest)))
    (maybe-remove-oldest!)
    (store:put path file digest exports-used)))

(define (maybe-remove-oldest!)
  ;; assumes called in with-semaphore from get-file or put
  (when (>= (hash-count cache) (current-cache-maximum-entries))
    (define-values (oldest-path _)
      (for/fold ([oldest-path #f]
                 [oldest-time +inf.0])
                ([(path entry) (in-hash cache)])
        (if (< (entry-time entry) oldest-time)
            (values path         (entry-time entry))
            (values oldest-path oldest-time))))
    (hash-remove! cache oldest-path)))
