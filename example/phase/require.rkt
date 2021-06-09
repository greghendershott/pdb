#lang racket/base

(require (for-syntax racket/base)
         "define.rkt")

(printf "0 = ~a\n" x)

(begin-for-syntax
  (printf "1 = ~a\n" x))
