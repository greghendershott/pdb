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
                             uses-of-export)))

(define-logger pdb-relations)

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
  (log-pdb-relations-debug "~v" `(use->def* ,use-path ,pos #:nominal? ,nominal? #:same-name? ,same-name?))
  (match (get-file use-path)
    [(? file? f)
     (define am (file-arrows f))
     (match (span-map-ref (arrow-map-use->def am) pos #f)
       [(? lexical-arrow? a)
        (log-pdb-relations-debug "  ~v" a)
        (and (or (not same-name?)
                 (eq? (lexical-arrow-use-sym a) (lexical-arrow-def-sym a)))
             (list use-path (arrow-def-beg a) (arrow-def-end a) 0))]
       [(or (? export-rename-arrow? a)
            (? import-rename-arrow? a))
        (log-pdb-relations-debug "  ~v" a)
        (if same-name?
            (list use-path (arrow-use-beg a) (arrow-use-end a) 0)
            (list use-path (arrow-def-beg a) (arrow-def-end a) 0))]
       [(? import-arrow? a)
        (log-pdb-relations-debug "~v" a)
        (match-define (cons def-path def-ibk) (if nominal?
                                                  (import-arrow-nom a)
                                                  (import-arrow-from a)))
        (let loop ([use-offset (- pos (arrow-use-beg a))]
                   [def-path def-path]
                   [def-ibk def-ibk])
          (and (path? def-path) ;not symbol like '#%runtime or '#%core
               (match (get-file def-path)
                 [(? file? f)
                  (define ht (if nominal?
                                 (file-pdb-exports f)
                                 (file-pdb-definitions f)))
                  (match (hash-ref ht def-ibk #f)
                    [(? list? sub-ranges)
                     (log-pdb-relations-debug "  ~v ~v" def-ibk sub-ranges)
                     (for/or ([v (in-list sub-ranges)])
                       (match-define (sub-range ofs span _sub-sym sub-pos) v)
                       (cond
                         [(and (<= ofs use-offset)
                               (< use-offset (+ ofs span)))
                          (match sub-pos
                            [(? number? sub-pos)
                             (log-pdb-relations-debug "  use-offset ~v so chose ~v from ~v"
                                                      use-offset v sub-ranges)
                             (list def-path sub-pos (+ sub-pos span) (- use-offset ofs))]
                            [(re-export p i)
                             (log-pdb-relations-debug "  use-offset ~v so chose ~v from ~v; adjusting use-offset to ~v"
                                                      use-offset v sub-ranges (- use-offset ofs))
                             (loop (- use-offset ofs) p i)]
                            [#f #f])]
                         [else
                          (log-pdb-relations-debug "~v: false"
                                                   `(hash-ref ,ht ,def-ibk #f))
                          #f]))]
                    [#f
                     (log-pdb-relations-debug "(get-file ~v): false" def-path)
                     #f])]
                 [#f #f])))]
       [#f
        (log-pdb-relations-debug "(span-map-ref file-arrows ~v): false" pos)
        #f])]
    [#f
     (log-pdb-relations-debug "(get-file ~v): false" use-path)
     #f]))

;; A wrapper for use-pos->def*, assuming #:nominal? #t and
;; #:same-name? #f. Single steps through the complete chain of name
;; introductions resulting from imports and exports, including through
;; renames.
(define/contract (nominal-use->def path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) exact-integer? exact-integer?)))
  (match (use->def* path pos #:nominal? #t #:same-name? #f)
    [(list p b e _o) (list p b e)]
    [#f #f]))

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
      [(== previous-answer) previous-answer]
      [(and answer (list def-path def-beg _def-end ofs))
       (loop answer def-path (+ def-beg ofs))]
      [#f previous-answer])))

;; A wrapper for use->def/fix-point, using false for #:nominal? and
;; #:same-name? -- i.e. "jump all the way to actual definition".
(define/contract (use->def path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) position? position?)))
  (match (use->def/fix-point path pos #:nominal-and-same-name? #f)
    [(list p b e _o) (list p b e)]
    [#f #f]))

;; A wrapper for use->def/fix-point, using true for #:nominal? and
;; #:same-name? -- i.e. "find the most distant same-named nominal
;; definition".
(define/contract (use->def/same-name path pos)
  (-> (and/c path? complete-path?) position?
      (or/c #f (list/c (and/c path? complete-path?) position? position?)))
  (match (use->def/fix-point path pos #:nominal-and-same-name? #t)
    [(list p b e _o) (list p b e)]
    [#f #f]))

;; Used by `rename-sites` to handle the case where it is given a def
;; site as opposed to a use site -- so if use->def/same-name fails,
;; try this.
(define (def->def/same-name path pos)
  (define f (get-file path))
  (or (for/or ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set)))])
        (and (lexical-arrow? a)
             (eq? (lexical-arrow-use-sym a) (lexical-arrow-def-sym a))
             (list path (arrow-def-beg a) (arrow-def-end a))))
      ;; Currently we don't have arrows on prefix-out prefixes, so
      ;; also check whether the site is a sub-range of an export.
      (for/or ([sub-ranges (in-hash-values (file-pdb-exports f))])
        (for/or ([v (in-list sub-ranges)])
          (match-define (sub-range _ofs span _sub-sym sub-pos) v)
          (and (number? sub-pos)
               (<= sub-pos pos)
               (< pos (+ sub-pos span))
               (list path sub-pos (+ sub-pos span)))))))

;; Same-named def->uses. Follows nominal chain, in reverse.
(define/contract (def->uses/same-name path def-beg def-end)
  (-> (and/c path? complete-path?) position? position?
      any #;(hash/c (and/c path? complete-path?) (listof (cons/c position? position?))))
  (unless (< def-beg def-end)
    (error 'def->uses/same-name
           "expected def-beg < def-end\n  def-beg: ~v\n  def-end: ~v\n"
           def-beg def-end))

  (define ht (make-hash))
  (define (add-use! path beg end)
    (hash-update! ht
                  path
                  (λ (s) (set-add s (cons beg end)))
                  (set)))

  (add-use! path def-beg def-end)
  (define f (get-file path))
  (define positions
    (for/fold ([positions (set def-beg)])
              ([a (in-set (span-map-ref (arrow-map-def->uses (file-arrows f))
                                        def-beg
                                        (set)))])
      (cond
        [(and (lexical-arrow? a)
              (eq? (lexical-arrow-def-sym a) (lexical-arrow-use-sym a)))
         (add-use! path (arrow-use-beg a) (arrow-use-end a))
         (set-add positions (arrow-use-beg a))]
        [else positions])))

  ;; Note: This relies on use->def/same-name having already traversed
  ;; the import chain to the defining file, analyzing it (and each
  ;; other file along the way) if necessary, so that the db tables
  ;; will have the necessary informaton.
  (for ([pos (in-set positions)])
    (store:uses-of-export path pos add-use!))

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
;;
;; TODO: Status quo we tell the client the sites to change, and let it
;; make the changes. Subsequently, the client will tell us to
;; re-analyze each changed file. But that's inefficient, compared to
;; us simply updating our db with the new name and positions; the
;; /relations/ don't change. So instead there could be some flow
;; where, asssuming the user proceeds and supplies the new name, it
;; gives us back the same list of sites, plus the new name, and we
;; proactively make the change on the db side. We would also need to
;; update the new digest for each file, somehow, to avoid unnecesary
;; re-analysis happening after all.
(define/contract (rename-sites path pos)
  (-> (and/c path? complete-path?) position?
      any #;(hash/c (and/c path? complete-path?) (listof (cons/c position? position?))))
  ;; Find the def site, which might already be at `pos`.
  (match (or (use->def/same-name path pos)
             (def->def/same-name path pos))
    [(list def-path def-beg def-end)
     (def->uses/same-name def-path def-beg def-end)]
    [#f (make-hash)]))
