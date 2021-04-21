#lang racket/base

(require racket/format
         racket/match
         racket/set
         racket/sequence
         "common.rkt")

(provide analyze-imports-and-exports)

;; Two purposes here:
;;
;; 1. Find completion candidates from imports. Similar to what
;; imports.rkt does in Racket Mode back end.
;;
;; 2. Discover places where an export or import introduces a new name.
;; Although things like identifier-binding elide these renames, which
;; is great for most purposes, some commands (e.g. automatic renaming)
;; need to be aware of the "scope" of such an alias.
;;
;; For now, let's aside aside contract-out, and develop a solid
;; understanding of renames in terms of require and provide forms.
;;
;; When a module imports a binding, it can rename it: The `require`
;; form can introduce a new name, introduce an alias. Furthermore, the
;; module that proximally exported that binding may have introduced a
;; new name in its `provide` form. Furthermore, unless that exporting
;; module contains the definition, it may have imported it (possibly
;; renaming) from some other module that exports it (possibly
;; renaming). And so on indefinitely.
;;
;; Normally things like free-identifier=? and identifier-binding
;; traverse this entire chain of aliases, considering or returning
;; just the two ends of the chain. And normally that's desirable.
;; However, when the user wants to rename something, either manually
;; or via some automatic command, it's necessary to consider the
;; points in the chain where a new name is introduced. User renames
;; must be limited to segments of an alias chain that share the same
;; name.
;;
;; Assume a command given the location of a definition, and a new
;; name; it should rename the definition as well as /relevant/ uses.
;; The idea being the after the rename command finishes, there will
;; not be any compilation error. That command needs to limit itself to
;; replacing things that were /NOT/ renamed by a provide or require.
;; Those provide/require renames "break the chain" -- nothing
;; "downstream" of them should be renamed by the command.
;;
;; Such a command /could/ offer to work when given, not the location
;; of a defintion, by instead the location of a renaming provide or
;; require clause. The location could be treated much like a
;; definition, as the start of a chain of uses. Note that
;; drracket/check-syntax does draw arrows from identifiers to require
;; and provide forms. However, it does /not/ draw an arrow from the
;; "old" and "new" portions of a renaming sub-clause. We should
;; consider adding that?
;;
;; Another wrinkle: #%require has `prefix` and `prefix-all-except`
;; clauses. Ditto #%provide and its `prefix-all-defined` and
;; `prefix-all-defined-except` clauses. syncheck:add-arrow gives us
;; two arrows; for the prefix and the suffix. Each can be renamed or
;; not, independently.
;;
;; Note that the "granularity" here is higher than
;; syncheck:add-jump-to-defnition, which relies on identifier-binding.
;; That gives you the "ends of the chain". What we need here needs to
;; consider the full chain, or at least the ends of the chain after
;; being "trimmed" to exclude provide/require renames.
;;
;; ISSUE: drracket/check-syntax syncheck:add-jump-to-definition tells
;; us the defining modpath of a use. We `add-use` that to our db,
;; /without/ needing to analyze that defining file immediately. If a
;; command needs the location of the definition within the file, only
;; then we do analyze it -- "lazily", on-demand.
;;
;; BUT in this case, we only know if the use of a name introduced
;; locally, or, its /proximate/ importing file.
;;
;; 0. If use is of something defined locally, the definition site is
;; also the name-introduction site.
;;
;; 1. If the use is of a name introduced by a renaming import, the
;; rename-in id stx is the name-intro site.
;;
;; 2. Otherwise, we need to analyze the proximate importing file. With
;; that file:
;;
;;    2(a). If the name is from a renaming export, the rename-out id
;;    stx is the name-intro site.
;;
;;    2(b). Otherwise go to step 0. Note this may recur indefinitely until
;;    we've found a name-intro site --- either some renaming import/export
;;    site or the ultimate definition site (which IIUC should always be the
;;    same as reported by identifier-binding).
;;
;; Note about mapping /from/ uses:
;;
;; Actual definitions: 1. When mapping uses to actual definitions,
;; identifer-binding immediately tells us the ultimate defining
;; modpath; all we lack is the location /within/ that file. So it's
;; sufficient to record the modpath, and analyze the defining file
;; later on-demand. 2. When doing the reverse -- given an actual
;; definition, what are all its uses -- we already know the answer; no
;; further analysis is necessary. [Sure, if we haven't analyzed
;; file-using-foo.rkt, at all, then searching for uses of foo will
;; miss that use. But we immediately know all uses of actual
;; definitions, among any set of analyzed files.]
;;
;; But the situation with name introductions is trickier: 1. When we
;; encounter a use, all we know is the /proximate/ file supplying the
;; name. We might need to chase down N files before discovering the
;; ultimate name introduction site. Either we do that chase eagerly,
;; which is expensive, OR we have to record the proximate file as an
;; incomplete/tentative answer, and do the chase later. 2. That
;; tentative status makes the reverse -- given a name introduction,
;; what are all its uses -- much worse. We can't find all the uses,
;; not even among a set of files that we have analyzed, until we've
;; fully resolved the uses from proximate to ultimate.
;;
;; Idea:
;;
;; 1. Continue to analyze files "lazily".
;;
;; 2. Have a "proximate?" flag to indicate a use isn't yet fully
;; resolved to a name-introduction. This is set true intially (unless
;; intro site is in the same file).
;;
;; 3. When analyzing each file, record its name-introduction sites.
;; Then query for all uses showing that file as proximate. Update each
;; to point instead to the newly-analyzed file. If that file has the
;; intro site, change use status from proximate to ultimate.
;; Otherwise, leave the new proximate file, for a subsequent file
;; analysis to advance the resolution futher -- to yet another
;; proximate file, and eventually resolved to the ultimate site.
;;
;; As mentioned above, even a plain old "find all uses of an actual
;; definition" command is subject to not knowing about files that were
;; not analyzed at all. A "find all uses of a name-introduction site"
;; command has the further challenge that any use still in a proximate
;; (not ultimate) state might belong to the set of correct answers,
;; but we don't know that yet. To avoid that, a command could do a
;; simple db query: Are there /any/ just-proximate uses at all? If so,
;; the command can't run with guaranteed accuracy, yet. (Such a
;; situation will probably correspond to a non-empty queue of files
;; remaining to be analyzed -- but I'm not 100% sure about that, yet.)
;; The command must do a full resolution across the entire db.
;;
;; TL;DR: Although uses of name-introduction sites seems to require an
;; "eager", "depth-first" analysis of files, we can in fact handle it
;; "lazily". Uses might remain in a proximate state, but each newly
;; analyzed file may advance some of those one step closer to the
;; ultimate state. In some sense the uses marked proximate are
;; "thunks" or "promises".
;;
;; ===> Note that any newly analyzed file might have a new use of a
;; name-introduction in a non-proximate file. That is, we need to
;; traverse the chain. So I think we still need some "resolve all"
;; function that does this. Furthermore, since it is only needed by
;; rename commands, we /could/ do /none/ of that work up-front. Wait
;; until some rename command actually needs to run. (Or maybe, wait
;; until we've reached an idle quiescent state, and do it. Or, start
;; doing it but be "interruptible".)

(define (analyze-imports-and-exports add-import add-export add-rename path stx)

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
         (add-rename path (submods mods) #'imported-id #'local-id 'import))]
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
             ;; TODO: Prexies. Note that check-syntax uses "sub-range
             ;; binders" to report distinct arrows for the prefix and
             ;; the suffix. We should probably account for that, here,
             ;; both so that uses match up, and also because the
             ;; prefix and suffix can be renamed indepedently. Ex:
             ;; (prefix-in PRE: racket/string) and use of
             ;; PRE:string-length means that a syncheck:add-arrow
             ;; between both "PRE:"s /and a separate/ arrow between
             ;; "string-length" and "racket/string". Furthermore, the
             ;; "PRE:"s could all be changed together validly without
             ;; changing the suffixes. And the "racket/string" suffix
             ;; could be changed without changing the prefix.
             (when prefix
               (define prefix-str (->str prefix))
               (for ([old (in-set orig)])
                 (define new (~a prefix-str old))
                 (add-import path (submods mods) new)
                 ;; TODO: use sub-range-binders prop like check-syntax
                 #;
                 (add-rename-arrow path (submods mods) old new)))]
            [else
             (define prefix-str (if prefix (->str prefix) ""))
             (for ([old (in-set orig)])
               (define new (~a prefix-str old))
               (add-import path (submods mods) new)
               ;; TODO: use sub-range-binders prop like check-syntax
               #;
               (when prefix
                 (add-rename-arrow path (submods mods) old new)))])))

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
         (add-export path (submods mods) (->str #'export-id))
         ;; Note that for contract-out, what's happening here is
         ;; exporing the _wrapper_ renamed as the same name as the
         ;; wrapee; and, both IDs share the same srcloc.
         (add-rename path (submods mods) #'local-id #'export-id 'export))]
      [(struct struct-id (field-id ...))
       (let ([struct-id-str (->str #'struct-id)])
         (add-export path (submods mods) struct-id-str)
         (add-export path (submods mods) (~a "make-" struct-id-str))
         (add-export path (submods mods) (~a "struct:" struct-id-str))
         (add-export path (submods mods) (~a struct-id-str "?"))
         (for ([field-id (in-syntax #'(field-id ...))])
           (define field-id-str (->str field-id))
           (add-export path (submods mods) (~a struct-id-str "-" field-id-str))
           (add-export path (submods mods) (~a "set-" struct-id-str "-" field-id-str "!"))))]
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
      [(prefix-all-defined . _)        (void)] ;call add-rename
      [(prefix-all-defined-except . _) (void)] ;call add-rename
      [id
       (identifier? #'id)
       (add-export path (submods mods) (->str #'id))]))

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
