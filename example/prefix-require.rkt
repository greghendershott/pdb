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
