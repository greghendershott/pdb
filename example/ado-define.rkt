#lang racket/base
(define-values (a b) (values 42 42))
(provide (except-out (all-defined-out) b))
