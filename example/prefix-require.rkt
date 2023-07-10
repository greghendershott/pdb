#lang racket/base
(require "prefix-define.rkt"
         (prefix-in IN: "prefix-define.rkt"))
A:a
ALL:a
ALL:b
IN:A:a
IN:ALL:a
IN:ALL:b
NESTED:PREFIXES:FUN:c
(require (prefix-in OUTER: (prefix-in INNER: "prefix-define.rkt")))
OUTER:INNER:d
OUTER:INNER:NESTED:PREFIXES:FUN:c
(let ([OUTER:INNER:d 42]) ;red herring
  OUTER:INNER:d)
pre:re
zoom-apples-field
(require racket/require
         (multi-in "multi" ("1.rkt" "2.rkt")))
m1
multi-1:m1
m2
multi-2:m2
outer:inner:x
