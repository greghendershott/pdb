#lang racket/base

(require drracket/check-syntax
         "common.rkt")

(provide assert-drracket-adds-definition-targets-for-contract-wrappers!)

;; As of the following commits, we will get
;; syncheck:add-definition-target for the contract wrappers defined by
;; contract-out:
;;
;; 1. commit 274e4b9 in racket/racket
;; <https://github.com/racket/racket/commit/274e4b95b14cfe4a1c9733483dcc255ad0588f34>
;; 2. commit 67740df in drracket
;; <https://github.com/racket/drracket/commit/67740dfcc5e4630fd96c5539171b5385c0abfb0f>
;;
;; See: https://github.com/racket/racket/issues/3733
;;
;; Before that, we issue a clear error message and abend.

(define (drracket-adds-definition-targets-for-contract-wrappers?)
  (for/or ([v (in-list
               (show-content #'(module m racket/base
                                 (require racket/contract)
                                 (define (f x) x)
                                 (provide (contract-out [f (-> number? any)])))))])
    (and (memq (vector-ref v 0)
               '(syncheck:add-definition-target/phase-level+space
                 syncheck:add-definition-target))
         (equal? (symbol->string (vector-ref v 3))
                 "provide/contract-id-f.1"))))

(define msg "The versions of racket and/or drracket-tool-text-lib do not add definition targets for contract wrappers!")

(define (assert-drracket-adds-definition-targets-for-contract-wrappers!)
  (unless (drracket-adds-definition-targets-for-contract-wrappers?)
    (log-pdb-error msg)
    (error 'pdb msg)))
