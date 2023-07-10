#lang racket/base
(define a 42)
(define b 42)
(provide (prefix-out A: a)
         (prefix-out ALL: (all-defined-out)))
(define c 42)
(provide (prefix-out NESTED: (prefix-out PREFIXES: (prefix-out FUN: c))))
(define d 42)
(provide d)

(require "prefix-all-from-source.rkt")
(provide (prefix-out pre: (all-from-out "prefix-all-from-source.rkt")))

(struct apples (field))
(provide (prefix-out zoom- (struct-out apples)))

(module m racket/base
  (module m racket/base
    (define x 42)
    (provide (prefix-out inner: x)))
  (require 'm)
  (provide (prefix-out outer: inner:x)))
(require 'm)
outer:inner:x
(provide outer:inner:x)
