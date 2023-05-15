;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/format
         racket/match
         syntax/modresolve
         (prefix-in cache: (only-in "cache.rkt" get-file))
         "data-types.rkt")

(provide module-import-spec
         resolve-import-set)

;; Originally we would use module->exports to build a list of imported
;; symbols, while doing the analysis of fully expanded syntax, and
;; write the list of symbols to the db.
;;
;; But this meant storing a list of thousands of symbols in each file
;; analysis, and duplicating the same symbols for the same imported
;; modules across analyses. Furthermore, when we started to
;; distinguish the imported symbols for each module in a file (to
;; refine completion candidates), the redundancy grew even within each
;; analyzed file.
;;
;; So now, instead of eagerly getting/storing the list of symbols,
;; we're lazy; we just record an abstraction of the #%require form:
;; The module path, is it the module language, prefix if any, and
;; exceptions if any.
;;
;; To get the symbols, later: When the imported module is in a file
;; that we have already analyzed, we use that file's file-pdb-exports
;; to get the symbols. Otherwise, as a fallback, we can use
;; module->exports as we did before (except we're no longer running in
;; a namespace used for expansion, so we need to make a fresh
;; namespace and (dynamic-require _ (void)) to ensure the module is
;; "declared" (it need not be visited or instantiated)).

(define (module-import-spec path lang-stx raw-module-path-stx
                            #:prefix [prefix-stx #f]
                            #:except [exceptions #f])
  (define raw-module-path (syntax->datum raw-module-path-stx))
  ;; Resolve the module path so we needn't mess with current-directory
  ;; and/or current-load-relative-directory, later.
  (define rmp (resolve-module-path raw-module-path path))
  (define lang? (equal? raw-module-path (and lang-stx (syntax->datum lang-stx))))
  (define prefix (and prefix-stx (syntax->datum prefix-stx)))
  (list rmp lang? prefix exceptions))

(define (spec->symbols v)
  (match-define (list rmp lang? prefix exceptions) v)
  (define all-syms (resolved-module-path->symbols rmp))
  (define syms (set-subtract all-syms (or exceptions (seteq))))
  (define (prefixed)
    (for/seteq ([sym (in-set syms)])
      (string->symbol (~a prefix sym))))
  ;; If imports are from the module language, then {rename prefix}-in
  ;; /add aliases/, as well as the original names. Otherwise the
  ;; modified names /replace/ the original names.
  (if prefix
      (if lang?
          (set-union syms (prefixed))
          (prefixed))
      syms))

(module+ test
  (require (for-syntax racket/base)
           rackunit
           racket/runtime-path)
  (define-runtime-path modules.rkt (build-path "example" "modules.rkt"))
  (check-true (set-member? (spec->symbols
                            (module-import-spec modules.rkt #'racket/base #'net/url))
                           'get-pure-port))
  (check-false (set-member? (spec->symbols
                             (module-import-spec modules.rkt #'racket/base #'net/url
                                                 #:except (seteq 'get-pure-port)))
                           'get-pure-port)))

(define (resolved-module-path->symbols rmp)
  (or (using-db              rmp)
      (using-module->exports rmp)))

;; If we've already analyzed the file, we can examine its exports that
;; way. No need to dynamic-require and module->exports in a fresh
;; namespace.
(define (using-db rmp)
  (define (cp? v) (and (path? v) (complete-path? v)))
  (define-values (path mods)
    (match rmp
      [(? cp? p)                                     (values p null)]
      [(list 'submod (? cp? p) (? symbol? mods) ...) (values p mods)]
      [_ (values #f #f)]))
  (and path mods
       (match (cache:get-file path)
         [#f #f]
         [(? file? f)
          (for/fold ([s (seteq)])
                    ([ibk (in-hash-keys (file-pdb-exports f))])
            (if (equal? (ibk-mods ibk) mods)
                (set-add s (ibk-sym ibk))
                s))])))

;; As a fallback:
(define (using-module->exports rmp)
  (define-values (vars stxs)
    ;; with-handlers: Just ignore module paths module->exports can't
    ;; handle, including paths like 'foo or (submod "." _) or (submod
    ;; ".." _). drracket/check-syntax handles non-imported bindings;
    ;; our contribution is imported definitions.
    (with-handlers ([exn:fail? (λ _ (values null null))])
      (parameterize ([current-namespace (make-base-namespace)])
        (dynamic-require rmp (void))
        (module->exports rmp))))
  (for*/seteq ([vars+stxs    (in-list (list vars stxs))]
               [phase+spaces (in-list vars+stxs)]
               [export       (in-list (cdr phase+spaces))])
    (car export)))

(define (resolve-import-set symbols-or-specs [symbols (seteq)])
  (for/fold ([s symbols])
            ([v (in-set symbols-or-specs)])
    (match v
      [(? symbol? sym) (set-add s sym)]
      [(? list? spec)  (set-union s (spec->symbols spec))])))
