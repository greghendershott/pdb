#lang racket/base
(require "define.rkt")
plain
renamed
contracted1
contracted2
contracted/renamed
plain-by-macro
contracted-by-macro
sub
sub/renamed
foo
a-number
a-parameter
from-m
d/c
renamed-d/c
(module m racket/base
  (require (prefix-in PRE: "define.rkt"))
  PRE:plain
  PRE:renamed
  PRE:contracted/renamed
  ;; red herring for renames:
  (require (only-in "define.rkt"
                    [renamed plain]
                    [contracted/renamed c/r]))
  plain
  c/r
  (require (rename-in "define.rkt" [plain XXX]))
  XXX)
a-struct
a-struct?
a-struct-a
a-struct-b
(module m2 racket/base
  (require (rename-in "define.rkt" [renamed XXX]))
  XXX
  (provide (rename-out [XXX renamed])))
