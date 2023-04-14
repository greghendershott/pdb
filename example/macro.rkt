#lang racket/base
(require syntax/parse)
(define (go/one parse stx expr clauses)
  (define-syntax-class cl
    #:description "a clause with a pattern and a result"
    (pattern [p . rhs]
             #:with res (syntax/loc this-syntax [(p) . rhs])))
  (syntax-parse clauses
    #:context stx
    [(c:cl ...)
     (go parse stx (quasisyntax/loc expr (#,expr))
         #'(c.res ...))]))
(define (go . _)
  (void))
