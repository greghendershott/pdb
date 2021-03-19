#lang racket/base

(require drracket/check-syntax
         racket/format
         racket/sequence)

(provide maybe-use-hack-for-contract-wrappers)

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
;; Before that, we use a hack.

(define (analyze-provide/contract-transformers add-def path stx)

  (define (symbolic-compare? x y)
    (eq? (syntax-e x) (syntax-e y)))

  (define (handle-file-module stx)
    (syntax-case* stx (module) symbolic-compare?
      [(module _id _lang (_mb . es))
       (handle-module-level '() #'es)]))

  (define (handle-module-level submods es)
    (for ([e (in-syntax es)])
      (syntax-case* e
          (#%app
           define-syntaxes
           make-provide/contract-transformer
           make-provide/contract-arrow-transformer
           quote-syntax
           module
           module*)
          symbolic-compare?
        [(define-syntaxes
           (_)
           (#%app
            make-provide/contract-transformer
            (quote-syntax wrapper)
            (quote-syntax _)
            (quote-syntax original)
            . _))
         (add-def-for-contract path submods #'original #'wrapper)]
        [(define-syntaxes
           (_)
           (#%app
            make-provide/contract-arrow-transformer
            (quote-syntax wrapper)
            (quote-syntax _)
            (quote-syntax original)
            . _))
         (add-def-for-contract path submods #'original #'wrapper)]
        [(module id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [(module* id _lang . es)
         (handle-module-level (cons (syntax-e #'id) submods) #'es)]
        [_ (void)])))

  (define (add-def-for-contract path submods original wrapper)
    (define beg (syntax-position original))
    (define span (syntax-span original))
    (when (and beg span)
      (define wrapper-sym (string->symbol (~a (syntax-e wrapper) ".1")))
      (add-def path beg (+ beg span) submods wrapper-sym)))

  (handle-file-module stx))

(define (drracket-adds-definition-targets-for-contract-wrappers?)
  (for/or ([v (in-list
               (show-content #'(module m racket/base
                                 (require racket/contract)
                                 (define (f x) x)
                                 (provide (contract-out [f (-> number? any)])))))])
    (and (eq? (vector-ref v 0)
              'syncheck:add-definition-target)
         (equal? (symbol->string (vector-ref v 3))
                 "provide/contract-id-f.1"))))

(define maybe-use-hack-for-contract-wrappers
  (if (drracket-adds-definition-targets-for-contract-wrappers?)
      void
      analyze-provide/contract-transformers))
