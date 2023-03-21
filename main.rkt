#lang racket/base

(require data/interval-map
         racket/contract
         racket/match
         racket/set
         "analyze.rkt"
         "data-types.rkt"
         "span-map.rkt"
         (only-in "store.rkt"
                  [close store:close]
                  [get-file store:get-file]
                  read-file-from-sqlite
                  all-known-paths)
         (only-in "nominal-imports.rkt"
                  [close nominal-imports:close]
                  [lookup files-nominally-importing]))

(provide close
         analyze-path
         all-known-paths
         analyze-all-known-paths
         queue-directory-to-analyze

         get-annotations
         get-completion-candidates
         get-errors

         use->def
         nominal-use->def
         rename-sites)

(define (close)
  (store:close)
  (nominal-imports:close))

(define (get-file path)
  (match (store:get-file path)
    [(? file? f)
     #:when (file-digest f) ;not a queue-more-files-to-analyze stub
     f]
    [_ (analyze-path path)
       (or (store:get-file path)
           (error 'get-file
                  "~v\n No analysis available due to an error; see logger topic `pdb`."
                  path))]))

;;; Simple queries

;; Annotations pertain to specific spans. There are various kinds.
;; get-annotations returns all kinds mixed and sorted by position. It
;; supports a "windowed" query so e.g. an editor can get just
;; "screen-fulls" (or say jit-font-lock-window-fulls), to help with
;; larger files.
(define/contract (get-annotations path [beg min-position] [end max-position])
  (->* (complete-path?) (position? position?) any) ;returns pdb?
  (define f (get-file path))
  (define (def-sites)
    (for/list ([v (in-list (span-map-refs (arrow-map-def->uses (file-arrows f)) beg end))])
      (match-define (cons (cons def-beg def-end) uses) v)
      (define import? (import-arrow? (set-first uses)))
      (list 'def-site
            def-beg
            def-end
            import?
            (sort (for/list ([use (in-set uses)])
                    (list (arrow-use-beg use)
                          (arrow-use-end use)))
                  < #:key car))))
  (define (use-sites)
    (for/list ([v (in-list (span-map-refs (arrow-map-use->def (file-arrows f)) beg end))])
      (match-define (cons (cons use-beg use-end) a) v)
      (list 'use-site
            use-beg
            use-end
            (import-arrow? a)
            (arrow-def-beg a)
            (arrow-def-end a))))
  (define (mouse-overs)
    (for/list ([v (in-list (span-map-refs (file-mouse-overs f) beg end))])
      (match-define (cons (cons beg end) texts) v)
      (list 'mouse-over beg end texts)))
  (define (doc-sites)
    (for/list ([v (in-list (span-map-refs (file-docs f) beg end))])
      (match-define (cons (cons beg end) (cons path anchor)) v)
      (list 'doc-link beg end path anchor)))
  (define (unused-requires)
    (for/list ([v (in-list (span-map-refs (file-unused-requires f) beg end))])
      (match-define (cons (cons beg end) _) v)
      (list 'unused-require beg end)))
  (sort (append (def-sites)
                (use-sites)
                (mouse-overs)
                (doc-sites)
                (unused-requires))
        < #:key cadr))

;; Optionally accepts a position with the view that someday we'd build
;; a more-targeted data structure for this -- limited to /valid/
;; candidates within a module or even lexical scope. But for now we
;; continue the Racket Mode tradition of erring on the side of
;; offering more candidates, even if some aren't valid.
(define (get-completion-candidates path [_pos min-position])
  (define f (get-file path))
  (set-union
   (for/set ([v (in-set (file-imports f))]) ;-> immutable-set
     v)
   ;; ~= to getting candidates from syncheck:add-definition-target.
   (for/set ([v (in-hash-keys (file-defs f))])
     (ibk-sym v))
   ;; ~= to getting candidates from synchek:add-mouse-over messages
   ;; about "bound occurrence(s)", which includes lexical arrows, plus
   ;; more from our rename-arrows.
   (for/fold ([s (set)])
             ([uses (in-list (span-map-values (arrow-map-def->uses (file-arrows f))))])
     (match (set-first uses) ;assumption: all arrows to def are the same kind
       [(? lexical-arrow? a)
        (set-add s (lexical-arrow-sym a))]
       [(? rename-arrow? a)
        (set-add (set-add s (rename-arrow-old-sym a))
                 (rename-arrow-new-sym a))]
       [_ s]))))

;; Accepts no span or position on the theory that, when a file has one
;; or more errors, the user will always want to know and be able to go
;; to them, regardless of where they might be in the file.
(define (get-errors path)
  (for/list ([v (in-list (span-map->list (file-errors (get-file path))))])
    (match-define (list (cons beg end) (cons maybe-path message)) v)
    (list beg end
          (or maybe-path (path->string path))
          message)))

(module+ ex
  (require racket/path)
  (get-annotations (simple-form-path (build-path "example" "define.rkt")) 1500 1530)
  (get-annotations (simple-form-path (build-path "example" "typed-error.rkt")))
  (get-errors (simple-form-path (build-path "example" "typed-error.rkt")))
  (get-errors (simple-form-path (build-path "example" "require-error.rkt")))
  (get-completion-candidates (simple-form-path (build-path "example" "define.rkt"))))

(module+ test
  (require racket/runtime-path
           rackunit)
  (define-runtime-path require.rkt "example/require.rkt")
  (check-true
   (match (for/or ([a (in-list (get-annotations require.rkt 20 21))])
            (and (eq? 'use-site (car a))
                 a))
     [(list 'use-site 20 27 #t 7 18)
      (match (use->def require.rkt 20)
        [(list (? path?) 10485 10492) #t]
        [_ #f])]
     [_ #f])
   "We get a use-site with import? true, for `require`, and, use->def for that site gives the expected location in reqprov.rkt"))

;;; Queries involving uses and definitions

;; Given a file position, see if it is a use of a definition. If so,
;; return the definition location, else #f. i.e. This is the basis for
;; "find definition".
;;
;; One wrinkle here is that we may already know about a use of a
;; definition in file A, and know it is defined in file B, but we
;; haven't yet analyzed that defining file. We may need to go analyze
;; file B first, so we can find locations within.
;;
;; Another consideration is "how far to jump". When #:nominal? is
;; false, we use the identifier-binding from-xxx fields to find the
;; definition, which elides any arbitrarily long chain of renaming
;; imports and exports (but see use->def wrapper function, below, wrt
;; more jumps for e.g. contracts). Otherwise, when #:nominal? is true,
;; we use the nominal-from-xxx fields, letting arrows trace each such
;; step.
;;
;; Another wrinkle is "sub range binders". Although
;; drracket/check-syntax handles this for lexical arrows, we need to
;; do extra work for import arrows.
(define (use->def* use-path pos #:nominal? nominal? #:same-name? same-name?)
  (match (get-file use-path)
    [(struct* file ([arrows am]))
     (match (span-map-ref (arrow-map-use->def am) pos #f)
       [(? lexical-arrow? a)
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        #:when same-name?
        (list use-path (arrow-use-beg a) (arrow-use-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(? import-arrow? a)
        (match-define (cons def-path def-ibk) (if nominal?
                                                  (import-arrow-nom a)
                                                  (import-arrow-from a)))
        (let loop ([def-path def-path]
                   [def-ibk def-ibk])
          (match (get-file def-path)
            [(struct* file ([defs defs] [exports exports] [sub-range-binders srbs]))
             (match (hash-ref (if nominal? exports defs) def-ibk #f)
               [(cons (? path? p) (? ibk? i)) ;follow re-provide
                (if (and (equal? p def-path) (equal? i def-ibk))
                    #f
                    (loop p i))]
               [(cons (? position? beg) (? position? end))
                ;; When the external definition has sub-range-binders, refine
                ;; to where the arrow definition points, based on where within
                ;; the use `pos` is.
                (match (hash-ref srbs def-ibk #f)
                  [(? interval-map? srb-im)
                   (define a (span-map-ref (arrow-map-use->def am) pos #f))
                   (define offset (- pos (arrow-use-beg a)))
                   (match (interval-map-ref srb-im offset #f)
                     [(list beg end _) (list def-path beg end)]
                     [_ (list def-path beg end)])]
                  [#f (list def-path beg end)])]
               [#f #f])]
            [#f #f]))]
       [#f #f])]
    [#f #f]))

;; A wrapper for use-pos->def*, using #:nominal? #f. When the def site
;; is a use of another def, return that other def.
;;
;; This is to cover cases like contract-out, where identifier-binding
;; will take us to the contract _wrapper_, but we want the "full jump"
;; all the way to the definition wrapped by the contract. So we keep
;; calling use->def* until we arrive at a fix point.
(define/contract (use->def use-path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? position? position?)))
  (let loop ([previous-answer #f]
             [use-path use-path]
             [pos pos])
    (match (use->def* use-path pos #:nominal? #f #:same-name? #f)
      [(and this-answer (list def-path def-beg _def-end))
       (if (equal? this-answer previous-answer)
           this-answer
           (loop this-answer def-path def-beg))]
      [#f previous-answer])))

;; A wrapper for use-pos->def*, assuming #:nominal? #t. Single steps
;; through the complete chain of name introductions resulting from
;; imports and exports, including through renames.
(define/contract (nominal-use->def use-path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? exact-integer? exact-integer?)))
  (use->def* use-path pos #:nominal? #t #:same-name? #f))

;; Find the most distant same-named nominal definition.
(define/contract (use->def/same-name path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? position? position?)))
  (let loop ([previous-answer #f]
             [path path]
             [pos pos])
    (match (use->def* path pos #:nominal? #t #:same-name? #t)
      [(and this-answer (list def-path def-beg def-end))
       ;; Handle the hacky negative "positions" by uing them as a
       ;; stepping stone to the next point, but ignoring them as a
       ;; possible final answer to be returned.
       (if (equal? this-answer previous-answer)
           this-answer
           (loop (if (and (position? def-beg) (position? def-end))
                     this-answer
                     previous-answer)
                 def-path
                 def-beg))]
      [#f previous-answer])))

;; Is <path pos> a definition site?
;;
;; Used by `rename-sites` to handle the case where it is given a def
;; site as opposed to a use site -- so if use->def/same-name fails,
;; try this.
(define (def->def/same-name path pos)
  (define f (get-file path))
  (or (for/or ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
        (and (lexical-arrow? a)
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; check-syntax might not draw an error to an identifer used in
      ;; a macro definition, as with e.g. `plain-by-macro` or
      ;; `contracted-by-macro` in example/define.rkt. Treat these as
      ;; definition sites anyway.
      (for/or ([beg+end-or-path+ibk (in-hash-values (file-exports f))])
        (match beg+end-or-path+ibk
          [(cons (? position? beg) (? position? end))
           (and (<= beg pos)
                (< pos end)
                (list path beg end))]
          [(cons (? path?) (? ibk?))
           ;; something to do here besides just ignore??
           #f]))
      (for/or ([b+e (in-hash-values (file-defs f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define/contract (def->uses/same-name path beg end)
  (-> complete-path? position? position?
      any #;(hash/c complete-path? (listof (cons/c position? position?))))
  #;(println (list 'def->uses/same-name def-path pos))

  (define (find-uses-in-other-files-of-exports-defined-here f path pos)
    (for ([(ibk def) (in-hash (file-exports f))])
      (match def
        [(cons (? position? def-beg) (? position? def-end))
         (when (and (<= def-beg pos) (< pos def-end))
           (find-uses-of-export (cons path ibk)))]
        [_ (void)])))

  (define (find-uses-of-export path+ibk)
    (for ([path (in-list (files-nominally-importing path+ibk))])
      (define f (read-file-from-sqlite path)) ;get w/o touching cache
      ;; Does this file anonymously re-export the item? If so, go look
      ;; for other files that import it as exported from this file.
      (for ([(export-ibk import-path+ibk) (in-hash (file-exports f))])
        (when (equal? path+ibk import-path+ibk)
          (find-uses-of-export (cons path export-ibk))))
      ;; Check for uses of the export in this file, then see if each
      ;; such use is in turn an export used by other files.
      (for ([a (in-list (arrow-map-arrows (file-arrows f)))])
        (when (and (import-arrow? a)
                   (equal? (import-arrow-nom a) path+ibk))
          (add! path (arrow-use-beg a) (arrow-use-end a))
          (find-uses-in-file path (arrow-use-beg a))
          (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))))))

  (define (find-uses-in-file path pos)
    (define f (get-file path))

    (for ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
      (unless (rename-arrow? a)
        (add! path (arrow-use-beg a) (arrow-use-end a))
        ;; See if use site is an exported definition that is imported
        ;; and used by other analyzed files.
        (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))))

    ;; For a rename-arrow we want to consider the newly introduced
    ;; name -- e.g. the `new` in `(rename-in m [old new])` -- which is
    ;; the "use" position.
    (match (span-map-ref (arrow-map-use->def (file-arrows f)) pos #f)
      [(? rename-arrow? a)
       (add! path (arrow-use-beg a) (arrow-use-end a))
       ;; See if use site is an exported definition that is imported
       ;; and used by other analyzed files.
       (find-uses-in-other-files-of-exports-defined-here f path (arrow-use-beg a))]
      [_ (void)])

    ;; See if <path pos> is site is an exported definition that is
    ;; imported and used by other analyzed files. Necessary for e.g.
    ;; all-defined-out.
    (find-uses-in-other-files-of-exports-defined-here f path pos))

  (define ht (make-hash)) ; path? => (set (cons beg end))
  (define (add! path beg end)
    (hash-update! ht
                  path
                  (λ (s) (set-add s (cons beg end)))
                  (set)))

  (add! path beg end)
  (find-uses-in-file path beg)
  (for/hash ([(p s) (in-hash ht)])
    (values p (sort (set->list s) < #:key car))))

;; Given a path and position, which may be either a use or a def,
;; return the set of places that must be renamed (the def site as well
;; as all the use sites) if any of them are.
;;
;; Basically we follow the nominal same-name chain from the use to the
;; def (the "root"), then reverse and "fan out" to follow the tree
;; back out through all uses sharing a name. So, we can handle
;; scenarios like something named "foo", then imported/exported as
;; "bar", then imported/exported as "foo". Each of these three is
;; independent wrt renaming; changing the name for each subset has no
;; affect on the others (except maybe at the "ends", e.g. a rename-{in
;; out} where just the old or new name should change).
;;
;; TODO/IDEA: If we return the current/old name, or its beg/end, or
;; its span -- whatever -- then we need only return the beg of each
;; use site. The end will always be name-length positions after beg.
(define/contract (rename-sites path pos)
  (-> complete-path? position?
      any #;(hash/c complete-path? (listof (cons/c position? position?))))
  ;; Find the def site, which might already be at `pos`.
  (match (or (use->def/same-name path pos)
             (def->def/same-name path pos))
    [(list def-path def-beg def-end)
     (def->uses/same-name def-path def-beg def-end)]
    [#f (make-hash)]))

;; Provide some things for exploring interactively in REPL, for e.g.
;; our tests in example.rkt. Not intended to be part of the normal,
;; public API.
(module+ private
  (require data/interval-map
           "data-types.rkt"
           "span-map.rkt")
  (provide (all-from-out "data-types.rkt")
           (all-from-out "span-map.rkt")
           (all-from-out data/interval-map)
           get-file
           def->def/same-name
           use->def/same-name
           def->uses/same-name))
