;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

;;; Relations between definition and use sites; rename sites

(module+ test) ;see example.rkt for many tests using actual example files

(require racket/contract
         racket/match
         (only-in "analyze.rkt" get-file)
         "data-types.rkt"
         (prefix-in store:
          (only-in "store.rkt"
                   get-file
                   files-nominally-importing)))

(provide use->def
         nominal-use->def
         rename-sites)

;; Provide some things not necessarily intended to be part of public
;; API. One use: For exploring interactively in REPL when working with
;; tests in example.rkt, such as writing new tests or understanding
;; test failures.
(module+ private
  (provide def->def/same-name
           use->def/same-name
           def->uses/same-name))

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
;; do extra work for import arrows, where the sub range binders are on
;; the export in the other analyzed file.
(define (use->def* use-path pos #:nominal? nominal? #:same-name? same-name?)
  (match (get-file use-path)
    [(? file? f)
     (define am (file-arrows f))
     (match (span-map-ref (arrow-map-use->def am) pos #f)
       [(? lexical-arrow? a)
        (and (or (not same-name?)
                 (eq? (lexical-arrow-use-sym a) (lexical-arrow-def-sym a)))
             (list use-path (arrow-def-beg a) (arrow-def-end a)))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        (if same-name?
            (list use-path (arrow-use-beg a) (arrow-use-end a))
            (list use-path (arrow-def-beg a) (arrow-def-end a)))]
       [(? import-arrow? a)
        (define use-offset (- pos (arrow-use-beg a)))
        (match-define (cons def-path def-ibk) (if nominal?
                                                  (import-arrow-nom a)
                                                  (import-arrow-from a)))
        (let loop ([def-path def-path]
                   [def-ibk def-ibk])
          (and (path? def-path) ;not symbol like '#%runtime or '#%core
               (match (get-file def-path)
                 [(struct* file ([syncheck-definition-targets defs]
                                 [pdb-exports exports]
                                 [pdb-sub-ranges sub-ranges]))
                  (define (use->sub-def parts)
                    ;; refine to where the arrow definition points,
                    ;; based on where within the use `pos` is.
                    (match parts
                      [(? list? subs)
                       (for/or ([sub (in-list subs)])
                         (match-define (vector ofs span _sub-sym sub-pos) sub)
                         (cond
                           [(and (<= ofs use-offset)
                                 (< use-offset (+ ofs span)))
                            (match sub-pos
                              [(re-export p i)
                               (loop p i)]
                              [(? number? sub-pos)
                               (list def-path sub-pos (+ sub-pos span))]
                              [#f #f])]
                           [else #f]))]
                      [#f #f]))
                  (match (hash-ref (if nominal? exports defs) def-ibk #f)
                    [(cons beg end) ;definition
                     (or (use->sub-def (hash-ref sub-ranges def-ibk #f))
                         (list def-path beg end))]
                    [(simple-export beg end)
                     (or (use->sub-def (hash-ref sub-ranges def-ibk #f))
                         (list def-path beg end))]
                    [(re-export p i) ;follow re-provide
                     (if (and (equal? p def-path) (equal? i def-ibk))
                         #f
                         (loop p i))]
                    [(prefixed-export parts)
                     (use->sub-def parts)]
                    [#f #f])]
                 [#f #f])))]
       [#f #f])]
    [#f #f]))

;; A wrapper for use-pos->def*, assuming #:nominal? #t and
;; #:same-name? #f. Single steps through the complete chain of name
;; introductions resulting from imports and exports, including through
;; renames.
(define/contract (nominal-use->def path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) exact-integer? exact-integer?)))
  (use->def* path pos #:nominal? #t #:same-name? #f))

;; A wrapper for use->def*: When the def site is a use of another def,
;; return that other def.
;;
;; This is to cover cases like contract-out, where identifier-binding
;; will take us to the contract _wrapper_, but we want the "full jump"
;; all the way to the definition wrapped by the contract. So we keep
;; calling use->def* until we arrive at a fix point.
(define (use->def/fix-point path pos #:nominal-and-same-name? n&sn?)
  (let loop ([previous-answer #f]
             [use-path path]
             [pos pos])
    (match (use->def* use-path pos #:nominal? n&sn? #:same-name? n&sn?)
      [(and this-answer (list def-path def-beg _def-end))
       (if (equal? this-answer previous-answer)
           this-answer
           (loop this-answer def-path def-beg))]
      [#f previous-answer])))

;; A wrapper for use->def/fix-point, using false for #:nominal? and
;; #:same-name? -- i.e. "jump all the way to actual definition".
(define/contract (use->def path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) position? position?)))
  (use->def/fix-point path pos #:nominal-and-same-name? #f))

;; A wrapper for use->def/fix-point, using true for #:nominal? and
;; #:same-name? -- i.e. "find the most distant same-named nominal
;; definition".
(define/contract (use->def/same-name path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) position? position?)))
  (use->def/fix-point path pos #:nominal-and-same-name? #t))

;; Is <path pos> a definition site?
;;
;; Used by `rename-sites` to handle the case where it is given a def
;; site as opposed to a use site -- so if use->def/same-name fails,
;; try this.
(define (def->def/same-name path pos)
  (define f (get-file path))
  (or (for/or ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
        (and (lexical-arrow? a)
             (eq? (lexical-arrow-use-sym a) (lexical-arrow-def-sym a))
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; check-syntax might not draw an error to an identifer used in
      ;; a macro definition, as with e.g. `plain-by-macro` or
      ;; `contracted-by-macro` in example/define.rkt. Treat these as
      ;; definition sites anyway.
      (for/or ([v (in-hash-values (file-pdb-exports f))])
        (match v
          [(simple-export beg end)
           (and (<= beg pos)
                (< pos end)
                (list path beg end))]
          [(prefixed-export parts)
           (for/or ([v (in-list parts)])
             (match-define (vector _ofs span _sub-sym sub-pos) v)
             (and (number? sub-pos)
                  (<= sub-pos pos)
                  (< pos (+ sub-pos span))
                  (list path sub-pos (+ sub-pos span))))]
          [_ #f]))
      (for/or ([b+e (in-hash-values (file-syncheck-definition-targets f))])
        (match-define (cons beg end) b+e)
        (and (<= beg pos)
             (< pos end)
             (list path beg end)))))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define/contract (def->uses/same-name path beg end)
  (-> (and/c path? complete-path?) position? position?
      any #;(hash/c (and/c path? complete-path?) (listof (cons/c position? position?))))
  #;(println (list 'def->uses/same-name def-path pos))

  (define (find-uses-in-other-files-of-exports-defined-here f path pos)
    (for/or ([(ibk def) (in-hash (file-pdb-exports f))])
      (match def
        [(simple-export def-beg def-end)
         #:when (and (<= def-beg pos) (< pos def-end))
         (find-uses-of-export (cons path ibk) #f)
         #t]
        [(prefixed-export parts)
         (for/or ([v (in-list parts)])
           (match-define (vector ofs span _sub-sym sub-pos) v)
           (and (number? sub-pos)
                (<= sub-pos pos)
                (< pos (+ sub-pos span))
                (begin
                  (find-uses-of-export (cons path ibk) (cons ofs span))
                  #t)))]
        [_ #f])))

  (define (find-uses-of-export path+ibk maybe-sub-range-offset)
    (for ([path (in-list (store:files-nominally-importing path+ibk))])
      (define f (store:get-file path)) ;get w/o touching cache
      ;; Does this file anonymously re-export the item? If so, go look
      ;; for other files that import it as exported from this file.
      (for ([(export-ibk v) (in-hash (file-pdb-exports f))])
        (match v
          [(re-export (== (car path+ibk)) (== (cdr path+ibk)))
           (find-uses-of-export (cons path export-ibk) maybe-sub-range-offset)]
          ;; Although the prefix parts are N/A, the final (suffix)
          ;; part might be a re-export.
          [(prefixed-export parts)
           (match (reverse parts)
             [(cons (vector ofs span _sub_sym
                            (re-export (== (car path+ibk)) (== (cdr path+ibk))))
                    _)
              (find-uses-of-export (cons path export-ibk) (cons ofs span))]
             [_ (void)])]
          [_ (void)]))
      ;; Check for uses of the export in this file, then see if each
      ;; such use is in turn an export used by other files.
      (for ([a (in-list (arrow-map-arrows (file-arrows f)))])
        (when (and (import-arrow? a)
                   (equal? (import-arrow-nom a) path+ibk))
          (define-values (beg end)
            (match maybe-sub-range-offset
              [(cons ofs span) (values (+ (arrow-use-beg a) ofs)
                                       (+ (arrow-use-beg a) ofs span))]
              [#f              (values (arrow-use-beg a)
                                       (arrow-use-end a))]))
          (add! path beg end)
          (find-uses-in-file path beg)
          (find-uses-in-other-files-of-exports-defined-here f path beg)))))

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
  (-> (and/c path? complete-path?) position?
      any #;(hash/c (and/c path? complete-path?) (listof (cons/c position? position?))))
  ;; Find the def site, which might already be at `pos`.
  (match (or (use->def/same-name path pos)
             (def->def/same-name path pos))
    [(list def-path def-beg def-end)
     (def->uses/same-name def-path def-beg def-end)]
    [#f (make-hash)]))
