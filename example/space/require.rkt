#lang racket/base

(module m1 racket/base
  (require "define.rkt")
  kettle)

(module m2 racket/base
  (require (only-space-in soup "define.rkt"))
  (require (for-syntax racket/base))
  (define-syntax (in-space stx)
    (syntax-case stx ()
      [(_ space id) #`(quote-syntax
                       #,((make-interned-syntax-introducer (syntax-e #'space))
                          (syntax-local-introduce (datum->syntax #f (syntax-e #'id)))))]))
  (in-space soup kettle))
