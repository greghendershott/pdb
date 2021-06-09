#lang racket/base
(require (for-syntax racket/base))

(module m racket/base
  (require (for-syntax racket/base))

  (define x 0)
  (provide x)

  (begin-for-syntax
    (define x 1)
    (provide x)))

(require 'm)
(printf "0 = ~a\n" x)

(begin-for-syntax
  (printf "1 = ~a\n" x))
