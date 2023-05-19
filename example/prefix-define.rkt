#lang racket/base
(define a 42)
(define b 42)
(provide (prefix-out A: a)
         (prefix-out ALL: (all-defined-out)))
(define c 42)
(provide (prefix-out NESTED: (prefix-out PREFIXES: (prefix-out FUN: c))))
