#lang info

(define deps '(["base" #:version 8.7]
               ["drracket-tool-text-lib" #:version 1.3]))
(define build-deps '("rackunit-lib"))
(define test-omit-paths '("example/"))
(define clean '("compiled"))
