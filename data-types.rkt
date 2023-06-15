;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         data/interval-map
         racket/dict
         racket/set
         "span-map.rkt")

(require racket/lazy-require)
(lazy-require ["extra-arrows.rkt" (file-add-arrows)])

(provide (all-from-out data/interval-map
                       racket/dict
                       racket/set
                       "span-map.rkt")
         position?
         (struct-out ibk)
         (struct-out arrow)
         (struct-out lexical-arrow)
         (struct-out rename-arrow)
         (struct-out export-rename-arrow)
         (struct-out import-rename-arrow)
         (struct-out import-arrow)
         (struct-out lang-import-arrow)

         (struct-out sub-range)
         (struct-out re-export)

         (struct-out import-rename)

         make-arrow-map
         (struct-out arrow-map)
         arrow-map-set!
         arrow-map-remove!
         arrow-map-arrows

         (struct-out syncheck-docs-menu)
         (struct-out syncheck-arrow)
         (struct-out syncheck-jump)
         (struct-out syncheck-prefixed-require-reference)
         (struct-out file)
         make-file
         file-before-serialize
         file-after-deserialize)

;; We use 1-based positions just like syntax-position (but unlike
;; drracket/check-syntax).
(define position? exact-positive-integer?)

;; identifier-binding uniquely refers to a non-lexical binding via a
;; tuple of <path mods phase symbol>. Often we need all but the path
;; since we're working on things per-file. A struct for that:
(struct ibk (mods phase sym) #:prefab)

;; An arrow always has both ends in the same file. (Arrows for
;; imported definitions point to e.g. the `require` or module language
;; syntax in the importing file. For such arrows, the derived struct
;; `import-arrow`, below, also says in what other file to look.)
(struct arrow (phase use-beg use-end def-beg def-end) #:prefab)

(struct lexical-arrow arrow (use-sym def-sym) #:prefab)

(struct rename-arrow arrow (old-sym new-sym) #:prefab)
(struct export-rename-arrow rename-arrow () #:prefab)
(struct import-rename-arrow rename-arrow () #:prefab)

;; `from` and `nom` correspond to identifier-binding-resolved fields
;; Both are (cons path? ibk?), and are used to look up in hash-tables
;; stored in a `file` struct's fields `syncheck-definition-targets` or
;; `pdb-exports`, respectively.
(struct import-arrow arrow (sym from nom) #:prefab)
(struct lang-import-arrow import-arrow () #:prefab)

;; A non-empty list of sub-range structs is the value used in both the
;; file-pdb-definitions and file-pdb-exports hash-tables, as well
;; as the maybe-prefix-ranges field of the import-rename struct.
(struct sub-range
  (offset   ;natural
   span     ;natural
   sub-sym  ;symbol
   sub-pos  ;(or/c position? re-export)
   ) #:prefab)
(struct re-export (path ibk) #:prefab)

;; Value for file-pdb-import-renamex field
(struct import-rename
  (phase
   modpath-beg                ;natural
   modpath-end                ;natural
   maybe-prefix-ranges        ;(or/c #f (listof sub-range?))
   maybe-import-rename-arrow  ;(or/c #f import-rename-arrow?)
   ) #:prefab)

;; An arrow-map is a pair of span-maps, one for each "direction" of
;; def<->uses. (The same immutable arrow struct instance is stored in
;; both; IIUC this is just a pointer, not two copies.)
(struct arrow-map
  (def->uses  ;1:many (span-map def-beg def-end (set arrow))
   use->def)) ;1:1    (span-map use-beg use-end arrow)

(define (make-arrow-map [as null])
  (define m (arrow-map (make-span-map) (make-span-map)))
  (for ([a (in-list as)])
    (arrow-map-set! m a))
  m)

(define (arrow-map-arrows am)
  (span-map-values (arrow-map-use->def am)))

(define (arrow-map-set! am a)
  (span-map-add! (arrow-map-def->uses am)
                 (arrow-def-beg a)
                 (arrow-def-end a)
                 a)
  (span-map-set! (arrow-map-use->def am)
                 (arrow-use-beg a)
                 (arrow-use-end a)
                 a))

(define (arrow-map-remove! am a)
  (span-map-remove! (arrow-map-def->uses am)
                    (arrow-def-beg a)
                    (arrow-def-end a)
                    a)
  (span-map-remove! (arrow-map-use->def am)
                    (arrow-use-beg a)
                    (arrow-use-end a)
                    a))

(struct syncheck-docs-menu (sym label path anchor anchor-text) #:prefab)
(struct syncheck-arrow (def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow use-sym def-sym rb) #:prefab)
(struct syncheck-jump (sym path mods phase) #:prefab)
(struct syncheck-prefixed-require-reference (prefix req-beg req-end) #:prefab)

(define-syntax (defstruct stx)
  (define-syntax-class field
    (pattern [name init pre-serialize post-deserialize])
    ;; Shorthand for fields whose types need no serialization adjustment.
    (pattern [name init]
             #:with pre-serialize #'values
             #:with post-deserialize #'values))
  (syntax-parse stx
    [(_ name [field:field ...+])
     #:with make (format-id #'name "make-~a" #'name #:source #'name)
     #:with before-serialize (format-id #'name "~a-before-serialize" #'name #:source #'name)
     #:with after-deserialize (format-id #'name "~a-after-deserialize" #'name #:source #'name)
     #:with (accessor ...) (for/list ([f (in-list (syntax->list #'(field.name ...)))])
                              (format-id f "~a-~a" (syntax->datum #'name) (syntax->datum f)))
     #'(begin
         (struct name (field.name ...) #:prefab)
         (define (make)
           (name field.init ...))
         (define (before-serialize orig)
           (name (field.pre-serialize (accessor orig)) ...))
         (define (after-deserialize orig)
           (define new (name (field.post-deserialize (accessor orig)) ...))
           (file-add-arrows new)
           new))]))

(defstruct file
  (;; The `arrows` field is created from a few of the other fields;
   ;; see current-file-add-arrows parameter. To save space, we
   ;; serialize it as #f. Upon deserialization we create an empty
   ;; arrow-map, then finally (after all other struct fields
   ;; deserialized) call current-file-add-arrows to populate it.
   [arrows (make-arrow-map) (λ _ #f) (λ _ (make-arrow-map))]

   ;; From check-syntax. Effectively "just record the method calls".
   [syncheck-arrows (mutable-set) set->list list->mutable-set] ;(set/c syncheck-arrow?)
   [syncheck-definition-targets (make-hash)] ;(hash/c ibk? (cons def-beg def-end))
   [syncheck-tail-arrows (mutable-set) set->list list->mutable-set] ;(set/c (list/c (or/c #f path?) integer? (or/c #f path?) integer?)
   [syncheck-jumps (make-span-map)]
   [syncheck-prefixed-requires (make-span-map)]
   [syncheck-mouse-overs (make-span-map)] ;also items from our expansion
   [syncheck-docs-menus (make-span-map)]
   [syncheck-unused-requires (make-span-map)]
   [syncheck-require-opens (make-span-map)]
   [syncheck-text-types (make-span-map)]

   ;; From our expansion
   [pdb-errors (make-span-map)]

   ;; From our extra, `analyze-more` pass
   [pdb-modules (make-interval-map) dict->list make-interval-map]
   [pdb-definitions (make-hash)] ;(hash ibk? (listof sub-range?))
   [pdb-exports (make-hash)] ;(hash ibk? (listof sub-range?))
   [pdb-imports (make-hash)] ;(hash (seteq (or/c symbol? spec)))
   [pdb-import-renames (make-hash)] ;(hash (list mod-beg mod-end new-sym) import-rename)
   [pdb-export-renames (mutable-set) set->list list->mutable-set] ;(set export-rename-arrow))
   ))

(define (list->mutable-set xs)
  (apply mutable-set xs))
