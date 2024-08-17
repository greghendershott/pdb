#lang info

(define collection "pdb")
(define version "0.1")
(define deps '(["base" #:version "8.9.0.1"]
               ["drracket-tool-text-lib" #:version "1.3"]
               "sql"
               "db-lib"
               "data-lib"
               "rackunit-lib"))
(define build-deps '("rackunit-lib"
                     "at-exp-lib"))
(define compile-omit-paths '("example/"))
(define test-omit-paths '("example/"))
(define clean '("compiled"))
(define raco-commands '(("pdb" pdb/cli "program database" #f)))
