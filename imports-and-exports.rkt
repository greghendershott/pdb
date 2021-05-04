#lang racket/base

(require racket/format
         racket/syntax
         racket/match
         racket/set
         racket/sequence)

(provide analyze-imports-and-exports)

;; Two purposes here:
;;
;; 1. Find completion candidates from imports. Similar to what
;; imports.rkt does in Racket Mode back end.
;;
;; 2. Add some arrows for renaming require and provide.

(define (analyze-imports-and-exports add-import add-export
                                     add-import-rename add-export-rename
                                     path stx)

  (define (handle-module mods stx)
    (syntax-case stx (module #%module-begin #%plain-module-begin #%require)
      [(module mod-id lang (#%module-begin e ...))
       (handle-module-level (cons (syntax-e #'mod-id) mods)
                            #'lang
                            #'(e ...))]
      [(module mod-id lang (#%plain-module-begin e ...))
       (handle-module-level (cons (syntax-e #'mod-id) mods)
                            #'lang
                            #'(e ...))]))

  (define (handle-module-level mods lang es)
    (add-imports-from-module-exports mods lang lang)
    (for ([e (in-syntax es)])
      (syntax-case* e (#%require #%provide module module*) symbolic-compare?
        [(#%require e ...)
         (for ([spec (in-syntax #'(e ...))])
           (handle-raw-require-spec mods lang spec))]
        [(#%provide e ...)
         (for ([spec (in-syntax #'(e ...))])
           (handle-raw-provide-spec mods spec))]
        [(module mod-id sub-mod-lang (_mb e ...))
         (handle-module-level (cons (syntax-e #'mod-id) mods)
                              #'sub-mod-lang
                              #'(e ...))]
        [(module* mod-id sub-mod-lang (_mb e ...))
         (handle-module-level (cons (syntax-e #'mod-id) mods)
                              (if (syntax-e #'sub-mod-lang)
                                  #'sub-mod-lang
                                  lang)
                              #'(e ...))]
        [ _ (void)])))

  (define (handle-raw-require-spec mods lang spec)
    (syntax-case* spec (for-meta for-syntax for-template for-label just-meta)
        symbolic-compare?
      [(for-meta _phase specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-require-spec mods lang spec))]
      [(for-syntax specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-require-spec mods lang spec))]
      [(for-template specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-require-spec mods lang spec))]
      [(for-label specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-require-spec mods lang spec))]
      [(just-meta _phase specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-raw-require-spec mods lang spec))]
      [raw-module-path
       (handle-phaseless-require-spec mods lang #'raw-module-path)]))

  (define (handle-phaseless-require-spec mods lang spec)
    (syntax-case* spec (only prefix all-except prefix-all-except rename)
        symbolic-compare?
      [(only _raw-module-path . ids)
       (for ([id (in-syntax #'ids)])
         (add-import path (submods mods) id))]
      [(prefix prefix-id raw-module-path)
       (add-imports-from-module-exports mods
                                        lang
                                        #'raw-module-path
                                        #:prefix #'prefix-id)]
      [(all-except raw-module-path . ids)
       (add-imports-from-module-exports mods
                                        lang
                                        #'raw-module-path
                                        #:except (syntax->string-set #'ids))]
      [(prefix-all-except prefix-id raw-module-path . ids)
       (add-imports-from-module-exports mods
                                        lang
                                        #'raw-module-path
                                        #:prefix #'prefix-id
                                        #:except (syntax->string-set #'ids))]
      [(rename raw-module-path local-id imported-id)
       (begin
         (when (eq? (syntax-e #'raw-module-path) (syntax-e lang))
           (add-import path (submods mods) (->str #'imported-id)))
         (add-import path (submods mods) (->str #'local-id))
         (add-import-rename path (submods mods) #'imported-id #'local-id
                            #'raw-module-path))]
      [raw-module-path
       (module-path? (syntax->datum #'raw-module-path))
       (add-imports-from-module-exports mods
                                        lang
                                        #'raw-module-path)]))

  (define (add-imports-from-module-exports mods
                                           lang
                                           raw-module-path
                                           #:except [exceptions (set)]
                                           #:prefix [prefix #f])
    ;; with-handlers: Just ignore module paths module->exports can't
    ;; handle, including paths like 'foo or (submod "." _) or (submod
    ;; ".." _). drracket/check-syntax handles non-imported bindings;
    ;; our contribution is imported definitions.
    (with-handlers ([exn:fail? void])
      (define-values (vars stxs)
        (module->exports (syntax->datum raw-module-path)))
      (define orig
        (for*/mutable-set ([vars+stxs (in-list (list vars stxs))]
                           [phases    (in-list vars+stxs)]
                           [export    (in-list (cdr phases))])
          (->str (car export))))
      (set-subtract! orig exceptions)
      ;; If imports are from the module language, then {except rename
      ;; prefix}-in /add/ aliases, as well as the original names.
      ;; Otherwise the modified names /replace/ the original names.
      (cond [(eq? (syntax-e raw-module-path) (syntax-e lang))
             (for ([v (in-set orig)])
               (add-import path (submods mods) v))
             (when prefix
               (define prefix-str (->str prefix))
               (for ([old (in-set orig)])
                 (define new (~a prefix-str old))
                 (add-import path (submods mods) new)))]
            [else
             (define prefix-str (if prefix (->str prefix) ""))
             (for ([old (in-set orig)])
               (define new (~a prefix-str old))
               (add-import path (submods mods) new))])))

  (define (handle-raw-provide-spec mods spec)
    (syntax-case* spec (for-meta for-syntax for-label protect)
        symbolic-compare?
      [(for-meta _phase specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-provide-spec mods spec))]
      [(for-syntax specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-provide-spec mods spec))]
      [(for-label specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-phaseless-provide-spec mods spec))]
      [(protect specs ...)
       (for ([spec (in-syntax #'(specs ...))])
         (handle-raw-provide-spec mods spec))]
      [spec
       (handle-phaseless-provide-spec mods #'spec)]))

  (define (handle-phaseless-provide-spec mods spec)
    (syntax-case* spec
        (rename struct all-from all-from-except
                all-defined all-defined-except
                prefix-all-defined prefix-all-defined-except
                protect
                expand)
        symbolic-compare?
      [(rename local-id export-id)
       (begin
         (add-export path (submods mods) #'export-id)
         ;; Note that for contract-out, what's happening here is
         ;; exporting the _wrapper_ renamed as the same name as the
         ;; wrapee; and, both IDs share the same srcloc.
         (add-export-rename path (submods mods) #'local-id #'export-id))]
      [(struct struct-id (field-id ...))
       (let ([struct-id-str (->str #'struct-id)])
         (add-export path (submods mods) struct-id-str)
         (add-export path (submods mods) (format-id #f "make-~a" struct-id-str #:source #'struct-id))
         (add-export path (submods mods) (format-id #f "struct:~a" struct-id-str #:source #'struct-id))
         (add-export path (submods mods) (format-id #f "~a?" struct-id-str #:source #'struct-id))
         (for ([field-id (in-syntax #'(field-id ...))])
           (define field-id-str (->str field-id))
           (add-export path (submods mods) (format-id #f "~a-~a" struct-id-str field-id-str #:source field-id))
           (add-export path (submods mods) (~a "set-~a-~a!" struct-id-str field-id-str #:source field-id))))]
      ;; TODO: all-from and all-from-except will need to do something
      ;; similar to add-imports-from-module-exports, just adding
      ;; imports instead of exports.
      [(all-from . _)        (void)]
      [(all-from-except . _) (void)]
      ;; TODO: all-defined, all-defined-except, prefix-all-defined,
      ;; and prefix-all-defined-except will need to query the db for
      ;; the definitions we already discovered via main check-syntax
      ;; analysis. Or, can we just use module->exports on ourself,
      ;; here?
      [(all-defined . _)               (void)]
      [(all-defined-except . _)        (void)]
      [(prefix-all-defined . _)        (void)] ;call add-export-rename
      [(prefix-all-defined-except . _) (void)] ;call add-export-rename
      [id
       (identifier? #'id)
       (add-export path (submods mods) #'id)]))

  (define (submods rev-mods)
    (if (pair? rev-mods)
        (cdr (reverse rev-mods))
        null))

  (define (symbolic-compare? x y)
    (eq? (syntax-e x) (syntax-e y)))

  (define (->str v)
    (match v
      [(? syntax?) (->str (syntax-e v))]
      [(? symbol?) (symbol->string v)]
      [(? string?) v]))

  (define (syntax->string-set s)
    (for/mutable-set ([s (in-syntax s)])
      (->str s)))

  (handle-module null stx))
