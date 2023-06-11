;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match
         racket/syntax
         racket/set
         racket/sequence
         racket/phase+space
         "import-symbols.rkt")

(provide analyze-more)

;; Extra analysis. After it settles down, some of this might end up as
;; a PR for drracket-tool-lib.
;;
;; Three purposes here:
;;
;; 1. Find completion candidates from imports. Similar to what
;;    imports.rkt does in Racket Mode back end.
;;
;; 2. Add some arrows for renaming require and provide.
;;
;; 3. Provide information about definition targets with
;;    sub-range-binders syntax properties.

(define (analyze-more add-module
                      add-definitions
                      add-export
                      add-imports
                      add-import-rename
                      path stx-obj)
  (define (symbolic-compare? x y)
    (eq? (syntax-e x) (syntax-e y)))

  (let p+s+mod-loop ([stx-obj stx-obj]
                     [p+s 0]
                     [p+s-of-enclosing-module 0]
                     [mods #f]
                     [lang #f])
    (define (p+s-loop sexp p+s)
      (p+s+mod-loop sexp p+s p+s-of-enclosing-module mods lang))
    (define (mod-loop sexp mod lang)
      (define (sub-mods mod) (if mods (cons mod mods) (list mod)))
      (p+s+mod-loop sexp 0
                    (phase+space+ p+s p+s-of-enclosing-module)
                    (if mod (sub-mods mod) mods)
                    lang))
    (define (loop sexp)
      (p+s+mod-loop sexp p+s p+s-of-enclosing-module mods lang))

    (syntax-case* stx-obj
        (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                        set! quote quote-syntax with-continuation-mark
                        #%plain-app #%top
                        define-values define-syntaxes begin-for-syntax
                        module module*
                        #%require #%provide #%declare #%expression)
        (λ (x y) (free-identifier=? x y p+s 0))
      [(#%plain-lambda args bodies ...)
       (for-each loop (syntax->list #'(bodies ...)))]
      [(case-lambda [argss bodiess ...]...)
       (for-each loop (syntax->list #'((bodiess ...) ...)))]
      [(if test then else)
       (begin
         (loop #'test)
         (loop #'then)
         (loop #'else))]
      [(begin bodies ...)
       (for-each loop (syntax->list #'(bodies ...)))]
      [(begin0 bodies ...)
       (for-each loop (syntax->list #'(bodies ...)))]
      [(let-values (bindings ...) bodies ...)
       (for-each loop (syntax->list #'(bodies ...)))]
      [(letrec-values (bindings ...) bodies ...)
       (for-each loop (syntax->list #'(bodies ...)))]
      [(set! var e)
       (loop #'e)]
      [(with-continuation-mark a b c)
       (begin
         (loop #'a)
         (loop #'b)
         (loop #'c))]
      [(#%plain-app pieces ...)
       (for-each loop (syntax->list #'(pieces ...)))]
      [(define-values vars b)
       (begin
         (add-definitions stx-obj (submods mods) p+s #'vars)
         (loop #'b))]
      [(define-syntaxes names exp)
       (begin
         (add-definitions stx-obj (submods mods) p+s #'names)
         (p+s-loop #'exp (phase+space+ p+s 1)))]
      [(begin-for-syntax exp ...)
       (for ([e (in-list (syntax->list #'(exp ...)))])
         (p+s-loop e (phase+space+ p+s 1)))]
      [(module m-name m-lang (mb bodies ...))
       (begin
         (define module-name (syntax-e #'m-name))
         (define submodules (if mods (submods (cons module-name mods)) null))
         (add-module path submodules (site path stx-obj) #f)
         (add-imports path submodules
                      (module-import-spec path #'m-lang #'m-lang))
         (for ([body (in-list (syntax->list #'(bodies ...)))])
           (mod-loop body module-name #'m-lang)))]
      [(module* m-name m-lang (mb bodies ...))
       (begin
         (define module-name (syntax-e #'m-name))
         (define submodules (if mods (submods (cons module-name mods)) null))
         ;; Wrinkle: module+ splicing means this module* form could
         ;; originate from multiple disjoint source spans. We need a
         ;; new syntax property added in racket/private/submodule.rkt
         ;; to handle this; see
         ;; https://github.com/racket/racket/pull/4646/files
         (match (syntax-property stx-obj 'origin-form-srcloc)
           [#f
            (add-module path submodules (site path stx-obj) (not (syntax-e #'m-lang)))]
           [prop
            (let loop ([prop prop])
              (match prop
                [(cons this more) (loop this) (loop more)]
                [(srcloc src _line _col pos span)
                 (when (and (equal? src path) pos span)
                   (add-module path submodules (cons pos (+ pos span)) (not (syntax-e #'m-lang))))]
                [(list) (void)]))])
         (when (syntax-e #'m-lang)
           (add-imports path submodules
                        (module-import-spec path #'m-lang #'m-lang)))
         (for ([body (in-list (syntax->list #'(bodies ...)))])
           (if (syntax-e #'m-lang)
               (mod-loop body
                         module-name
                         #'m-lang)
               (mod-loop (syntax-shift-phase-level body (phase+space-shift+ p+s -1))
                         ;; traversals.rkt uses #f here, but that
                         ;; seems wrong; certainly it borks knowing
                         ;; module names for nested module+
                         module-name
                         lang))))]

      ; top level or module top level only:
      [(#%require raw-require-specs ...)
       (let ()
         (define (handle-raw-require-spec spec)
           (let loop ([spec spec]
                      [p+s p+s])
             (define (add-to-p+s n) (phase+space+ p+s n))
             (syntax-case* spec
                 (for-meta for-syntax for-template for-label just-meta for-space just-space portal)
                 symbolic-compare?
               [(for-meta phase specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (add-to-p+s (syntax-e #'phase))))]
               [(for-syntax specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (add-to-p+s 1)))]
               [(for-template specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (add-to-p+s -1)))]
               [(for-label specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space #f
                                          (phase+space-space p+s))))]
               [(just-meta phase specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space (syntax-e #'phase)
                                          (phase+space-space p+s))))]
               [(for-space space specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space (phase+space-phase p+s)
                                          (syntax-e #'space))))]
               [(just-space space specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space (phase+space-phase p+s)
                                          (syntax-e #'space))))]
               [(portal id content)
                (void)]
               [spec
                (handle-phaseless-spec #'spec p+s)])))
         (define (handle-phaseless-spec spec p+s)
           (define adjusted-p+s (phase+space+ p+s p+s-of-enclosing-module))
           (syntax-case* spec (only prefix all-except prefix-all-except rename)
               symbolic-compare?
             [(only _raw-module-path . ids)
              (add-imports path (submods mods) (syntax->symbol-set #'ids))]
             [(prefix prefix-id raw-module-path)
              (add-imports path (submods mods)
                           (module-import-spec path lang
                                               #'raw-module-path
                                               #:prefix #'prefix-id))]
             [(all-except raw-module-path . ids)
              (add-imports path (submods mods)
                           (module-import-spec path lang
                                               #'raw-module-path
                                               #:except (syntax->symbol-set #'ids)))]
             [(prefix-all-except prefix-id raw-module-path . ids)
              (add-imports path (submods mods)
                           (module-import-spec path lang
                                               #'raw-module-path
                                               #:prefix #'prefix-id
                                               #:except (syntax->symbol-set #'ids)))]
             ;; Not only does this result from obvious surface require
             ;; clauses like rename-in or only-in, in which case the
             ;; new local-id has full srcloc in original program, it
             ;; can arise from non-trivial prefix-in, in which case
             ;; local-id srcloc will have no syntax-position or -span
             ;; but will have a syntax property revealing the srcloc
             ;; of the one or more prefixes and of the suffix.
             [(rename raw-module-path local-id imported-id)
              (begin
                (when (eq? (syntax-e #'raw-module-path) (syntax-e lang))
                  (add-imports path (submods mods)
                               (seteq (syntax->datum #'imported-id))))
                (add-imports path (submods mods)
                             (seteq (syntax->datum #'local-id)))
                (add-import-rename path (submods mods) adjusted-p+s
                                   #'imported-id #'local-id #'raw-module-path))]
             [raw-module-path
              (module-path? (syntax->datum #'raw-module-path))
              (add-imports path (submods mods)
                           (module-import-spec path lang
                                               #'raw-module-path))]))
         (for ([spec (in-list (syntax->list #'(raw-require-specs ...)))])
           (handle-raw-require-spec spec)))]

      ; module top level only:
      [(#%provide raw-provide-specs ...)
       (let ()
         (define (handle-raw-provide-spec spec)
           (let loop ([spec spec]
                      [p+s p+s])
             (syntax-case* spec (for-meta for-syntax for-label protect for-space)
                 symbolic-compare?
               [(protect specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec p+s))]
               [(for-meta phase specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space (syntax-e #'phase)
                                          (phase+space-space p+s))))]
               [(for-syntax specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space+ p+s 1)))]
               [(for-label specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space #f
                                          (phase+space-space p+s))))]
               [(for-space space specs ...)
                (for ([spec (in-list (syntax->list #'(specs ...)))])
                  (loop spec (phase+space (phase+space-phase p+s)
                                          (syntax-e #'space))))]
               [spec
                (handle-phaseless-spec #'spec p+s)])))
         (define (handle-phaseless-spec spec p+s)
           (syntax-case* spec
               (rename struct all-from all-from-except
                       all-defined all-defined-except
                       prefix-all-defined prefix-all-defined-except
                       protect
                       expand)
               symbolic-compare?
             ;; Not only does this result from obvious surface
             ;; `provide` clauses like rename-out, in which case the
             ;; new export-id has full srcloc, it can arise from
             ;; prefix-out, in which case export-id srcloc will have
             ;; no syntax-position or -span but will have a syntax
             ;; property revealing the srcloc of the one or more
             ;; prefixes and of the suffix.
             ;;
             ;; Note that for contract-out, what's happening here is
             ;; exporting the _wrapper_ renamed as the same name as the
             ;; wrapee; and, both IDs share the same srcloc.
             [(rename local-id export-id)
              (add-export path (submods mods) p+s #'export-id #'local-id)]
             [(struct struct-id (field-id ...))
              (begin
                (add-export path (submods mods) p+s #'struct-id)
                (add-export path (submods mods) p+s (format-id #f "make-~a"
                                                               #'struct-id
                                                               #:source #'struct-id))
                (add-export path (submods mods) p+s (format-id #f "struct:~a"
                                                               #'struct-id
                                                               #:source #'struct-id))
                (add-export path (submods mods) p+s (format-id #f "~a?"
                                                               #'struct-id
                                                               #:source #'struct-id))
                (for ([field-id (in-syntax #'(field-id ...))])
                  (add-export path (submods mods) p+s (format-id #f "~a-~a"
                                                                 #'struct-id #'field-id
                                                                 #:source field-id))
                  (add-export path (submods mods) p+s (format-id #f "set-~a-~a!"
                                                                 #'struct-id #'field-id
                                                                 #:source field-id))))]
             ;; Although the surface macros `all-from-out` and
             ;; `all-from-except-out` seem to expand directly to a set
             ;; of raw module paths (handled by the default `id` case
             ;; below), not to uses of `all-from` and
             ;; `all-from-except`, these latter are documented and
             ;; could be used. For instance Racket's private/base.rkt
             ;; uses them in a handwritten #%provide.
             [(all-from raw-module-path)
              (handle-all-from #'raw-module-path (seteq))]
             [(all-from-except raw-module-path . exceptions)
              (handle-all-from #'raw-module-path (syntax->symbol-set #'exceptions))]
             ;; As with all-from, the clauses all-defined,
             ;; all-defined-except, prefix-all-defined, and
             ;; prefix-all-defined-except don't seem to be used by
             ;; surface macros like all-defined-out, including in
             ;; combination with prefix-out and except-out.
             ;;
             ;; As with all-from, these primitive clauses are
             ;; documented and might be used in handwritten code.
             ;; Someday we should try to support these, here.
             [(all-defined . _)               (void)]
             [(all-defined-except . _)        (void)]
             [(prefix-all-defined . _)        (void)] ;and add-export-rename?
             [(prefix-all-defined-except . _) (void)] ;and add-export-rename?
             [id
              (identifier? #'id)
              (add-export path (submods mods) p+s #'id)]))
         (define (handle-all-from raw-module-path exceptions)
           (define-values (vars stxs)
             (with-handlers ([exn:fail? (λ _ (values null null))])
               (module->exports (syntax->datum raw-module-path))))
           (for* ([vars+stxs    (in-list (list vars stxs))]
                  [phase+spaces (in-list vars+stxs)]
                  [export       (in-list (cdr phase+spaces))])
             (define sym (car export))
             (unless (set-member? exceptions sym)
               (define stx (datum->syntax raw-module-path sym #f)) ;no srcloc
               (add-export path (submods mods) p+s stx))))
         (for ([spec (in-list (syntax->list #'(raw-provide-specs ...)))])
           (handle-raw-provide-spec spec)))]
      [_ (void)])))

(define (submods rev-mods)
  (if (pair? rev-mods)
      (cdr (reverse rev-mods))
      null))

(define (site path stx)
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (and pos
       span
       (syntax-e stx)
       (equal? (syntax-source stx) path)
       (cons pos (+ pos span))))

  (define (syntax->symbol-set stxs)
    (for/seteq ([stx (in-syntax stxs)])
      (syntax->datum stx)))
