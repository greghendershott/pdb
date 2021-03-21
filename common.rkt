#lang racket/base

(require racket/format
         syntax/parse/define)

(provide (all-defined-out))

;;; logger/timing

(define-logger definitions)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-definitions-debug "~a cpu | ~a real | ~a gc <= ~a"
                         (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;; serializing

(define (str v)
  (cond [(path? v)  (path->string v)]
        [(number? v) v]
        [else        (~a v)]))

(define (un-str s)
  (read (open-input-string s)))
