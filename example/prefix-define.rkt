#lang racket/base
(define a 42)
(define b 42)
(provide (prefix-out A: a)
         (prefix-out ALL: (all-defined-out)))
