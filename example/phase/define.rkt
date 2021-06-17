#lang racket/base

(require (for-syntax racket/base))

(define x 0)
(provide x)

(begin-for-syntax
  (define x 1)
  (provide x
           (rename-out [x x-renamed-out])))
