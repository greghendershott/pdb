;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/format
         racket/match
         syntax/modcollapse
         syntax/modresolve
         "data-types.rkt"
         (prefix-in store: (only-in "store.rkt"
                                    put-resolved-module-path-exports
                                    get-resolved-module-path-exports)))

(provide module-import-spec
         resolve-import-set)

(define (module-import-spec path submodules lang-stx raw-module-path-stx
                            #:prefix [prefix-stx #f]
                            #:except [exceptions #f])
  (define raw-module-path (syntax->datum raw-module-path-stx))
  ;; Collapse the module path to disambiguate relative module paths
  ;; like (submod "." m). The "." or ".." is replaced by a unique,
  ;; complete path.
  (define rel-to (if (null? submodules)
                     `(file ,(path->string path))
                     `(submod (file ,(path->string path)) ,@submodules)))
  (define collapsed-module-path (collapse-module-path raw-module-path rel-to))
  (define resolved-module-path (resolve-module-path collapsed-module-path path))
  ;; It would be wasteful to store a copy of the symbols in our
  ;; analysis data for importing file; there could even be copies for
  ;; multiple submodules within each file. Instead we store the
  ;; symbols once in the db keyed by the exporter's resolved module
  ;; path. (Why store in the db now; why not just call
  ;; module->exports, later? Now the module is already declared in the
  ;; current namespace as a result of expansion; declaring it later
  ;; could be very slow.)
  (define symbols (module-path->exported-symbols resolved-module-path))
  (unless (set-empty? symbols)
    (store:put-resolved-module-path-exports resolved-module-path symbols))
  (define lang? (equal? raw-module-path (and lang-stx (syntax->datum lang-stx))))
  (define prefix (and prefix-stx (syntax->datum prefix-stx)))
  (list resolved-module-path lang? prefix exceptions))

(define (spec->symbols spec)
  (match-define (list resolved-module-path lang? prefix exceptions) spec)
  (define all-syms (store:get-resolved-module-path-exports resolved-module-path))
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

(define (resolve-import-set symbols-or-specs [symbols (seteq)])
  (for/fold ([s symbols])
            ([v (in-set symbols-or-specs)])
    (match v
      [(? symbol? sym) (set-add s sym)]
      [(? list? spec)  (set-union s (spec->symbols spec))])))

(define (module-path->exported-symbols resolved-module-path)
  (define-values (vars stxs)
    ;; with-handlers: Just ignore module paths module->exports can't
    ;; handle, including symbols.
    (with-handlers ([exn:fail? (λ _ (values null null))])
      (module->exports resolved-module-path)))
  (for*/seteq ([vars+stxs    (in-list (list vars stxs))]
               [phase+spaces (in-list vars+stxs)]
               [export       (in-list (cdr phase+spaces))])
    (car export)))
