#lang racket/base

(require (for-syntax racket/base))
(require (for-space bisque (only-space-in soup "define.rkt"))
         "define.rkt")

kettle

(define-syntax (in-space stx)
  (syntax-case stx ()
    [(_ space id) #`(quote-syntax
                     #,((make-interned-syntax-introducer (syntax-e #'space))
                        (syntax-local-introduce (datum->syntax #f (syntax-e #'id)))))]))

(begin-for-syntax
  (define kettle (namespace-syntax-introduce (in-space bisque kettle)))
  kettle)
