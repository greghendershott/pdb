#lang racket
(require "define-foo.rkt")
(provide (all-from-out "define-foo.rkt"))
(require "define-bar.rkt")
(provide bar)
