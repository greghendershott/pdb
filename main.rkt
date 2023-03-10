#lang racket/base

(require data/interval-map
         racket/contract
         racket/dict
         racket/match
         racket/set
         racket/string
         "analyze.rkt"
         "data-types.rkt"
         "store.rkt")

(provide open
         close
         all-known-paths
         analyze-path
         analyze-all-known-paths
         queue-directory-to-analyze
         use->def
         nominal-use->def
         rename-sites
         current-rename-sites-prefix
         ;; TODO: Simple functions to fetch by-postion annotations
         ;; like mouse-overs, tail-arrows, and unused-requires.
         )


;;; Queries

(define (get-file path)
  (match (get-file* path)
    [(? file? f)
     #:when (file-digest f) ;not a queue-more-files-to-analyze stub
     f]
    [_ (analyze-path path)
       (or (get-file* path)
           (error 'get-file
                  "~v\n No analysis available due to an error; see logger topic `pdb`."
                  path))]))

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
    [(struct* file ([arrows as]))
     (match (interval-map-ref as pos #f)
       [(? lexical-arrow? a)
        (list use-path (arrow-def-beg a) (arrow-def-end a))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        #:when same-name?
        (define-values (beg end _) (interval-map-ref/bounds as pos))
        (list use-path beg end)]
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
                   (define-values (use-beg _use-end _) (interval-map-ref/bounds as pos))
                   (define offset (- pos use-beg))
                   (match (interval-map-ref srb-im offset #f)
                     [(list beg end _) (list def-path beg end)]
                     [_ (list def-path beg end)])]
                  [#f (list def-path beg end)])]
               [#f #f])]
            [#f #f]))]
       [#f #f])]
    [#f #f]))

;; Like use-pos->def*, but when #:nominal? is false, and when the def
;; loc is also a use of another loc, return that other def.
;;
;; This is to cover cases like contract-out, where identifier-binding
;; will take us to the contract _wrapper_, but we want the "full jump"
;; all the way to the definition wrapped by the contract. So we keep
;; calling use->def* until we arrive at a fixed point.
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

;; The step by step flavor, e.g. useful for full chain of name
;; introductions resulting from imports and exports.
(define/contract (nominal-use->def use-path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? exact-integer? exact-integer?)))
  (use->def* use-path pos #:nominal? #t #:same-name? #f))

;; Find the most distant same-named nominal definition. When pos
;; already is a definition, return its bounds.
(define/contract (use->def/same-name path pos)
  (-> complete-path? position?
      (or/c #f (list/c complete-path? position? position?)))
  (let loop ([previous-answer #f]
             [path path]
             [pos pos])
    (match (use->def* path pos #:nominal? #t #:same-name? #t)
      [(and this-answer (list def-path def-beg def-end))
       #:when (string-prefix? (path->string def-path)
                              (current-rename-sites-prefix))
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
  (or (for/or ([a (in-dict-values (file-arrows f))])
        (and (lexical-arrow? a)
             (<= (arrow-def-beg a) pos)
             (< pos (arrow-def-end a))
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; check-syntax might not draw an error to an identifer used in
      ;; a macro definition, as with e.g. `plain-by-macro` or
      ;; `contracted-by-macro` in example/define.rkt. Treat these as
      ;; definition sites anyway.
      (for/or ([b+e (in-hash-values (file-exports f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))
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
    (for-each-known-path
     (current-rename-sites-prefix)
     (λ (path f)
       ;; Does this file anonymously re-export the item? If so, go look
       ;; for other files that import it as exported from this file.
       (for ([(export-ibk import-path+ibk) (in-hash (file-exports f))])
         (when (equal? path+ibk import-path+ibk)
           (find-uses-of-export (cons path export-ibk))))
       ;; Check for uses of the export in this file, then see if each
       ;; such use is in turn an export used by other files.
       (for ([(use-span a) (in-dict (file-arrows f))])
         (when (and (import-arrow? a)
                    (equal? (import-arrow-nom a) path+ibk))
           (match-define (cons use-beg use-end) use-span)
           (add! path use-beg use-end)
           (find-uses-in-file path use-beg)
           (find-uses-in-other-files-of-exports-defined-here f path use-beg))))))

  (define (find-uses-in-file path pos)
    (define f (get-file path))
    (for ([(use-span a) (in-dict (file-arrows f))])
      (match-define (cons use-beg use-end) use-span)
      (when
          (cond
            [(rename-arrow? a)
             ;; For a rename-arrow we want to consider the newly
             ;; introduced name -- e.g. the `new` in `(rename-in m
             ;; [old new])` -- which is the "use" position.
             (and (<= use-beg pos) (< pos use-end))]
            [(lexical-arrow? a)
             ;; Otherwise condider the "def" position.
             (and (<= (arrow-def-beg a) pos) (< pos (arrow-def-end a)))]
            [else #f])
        (add! path use-beg use-end)
        ;; See if use site is an exported definition that is imported
        ;; and used by other analyzed files.
        (find-uses-in-other-files-of-exports-defined-here f path use-beg)))
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

;; When the database contains many file analyses -- for example the
;; main distribution plus a few packages can be on the order of 8,000
;; files -- rename-sites will be extremely slow searching all those
;; files for uses. This parameter allows limiting the search to files
;; under a certain prefix. For example a user could limit this to
;; files within a certain project.
;;
;; NOTE: A predicate would be more flexible. But we need to give
;; sqlite a "LIKE /path/to/%" clause. If a single prefix isn't
;; flexible enough, instead this could be a list of prefixes, and we
;; could do a series of such queries.
;;
;; NOTE: The speed issue per se could also be addressed by building
;; some index from path+ibk to path -- that is, an index from exports
;; to files that use the exports. This could make things somewhat
;; faster for the case where the user really does want to search all
;; 8,000 files. On the other hand, indexes aren't free; this would add
;; space, and, would shift some time spent from queries to updates. It
;; might be worthwhile. I'm leery of doing updates where analyze-path
;; takes an extra second or more, due to inserting too many rows in
;; the sqlite table, as with the originally design that used db tables
;; for everything.
(define current-rename-sites-prefix (make-parameter "/"))

;; Provide some things for exploring interactively in REPL, for e.g.
;; our tests in example.rkt. Not intended to be part of the normal,
;; public API.
(module+ private
  (require data/interval-map
           "data-types.rkt")
  (provide (all-from-out "data-types.rkt")
           (all-from-out data/interval-map)
           get-file
           def->def/same-name
           use->def/same-name
           def->uses/same-name
           arrow-use
           arrow-def)
  (define (arrow-use path pos)
    (interval-map-ref/bounds (file-arrows (get-file path)) pos))
  (define (arrow-def path pos)
    (for*/list ([(b+e a) (in-dict (file-arrows (get-file path)))]
                #:when (and (<= (arrow-def-beg a) pos)
                            (< pos (arrow-def-end a))))
      (list b+e a))))
