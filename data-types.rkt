;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         data/interval-map
         racket/dict
         racket/format
         racket/match
         racket/set
         "span-map.rkt"
         "common.rkt")

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

         (struct-out export)
         (struct-out simple-export)
         (struct-out prefixed-export)
         (struct-out re-export)

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
         file-after-deserialize
         file-add-arrows)

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

;; Values for the pdb-exports field
(struct export () #:prefab)
(struct simple-export export (def-beg def-end) #:prefab)
(struct prefixed-export export (parts) #:prefab)
(struct re-export export (path ibk) #:prefab)

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
(struct syncheck-arrow (def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow use-stx-datum use-sym def-sym rb) #:prefab)
(struct syncheck-jump (sym path mods phase) #:prefab)
(struct syncheck-prefixed-require-reference (prefix prefix-beg prefix-end) #:prefab)

(define-syntax (defstruct stx)
  (define-syntax-class field
    (pattern [name init pre-serialize post-deserialize])
    ;; Shorthand for fields whose types need no serialization adjustment.
    (pattern [name init]
             #:with pre-serialize #'values
             #:with post-deserialize #'values))
  (syntax-parse stx
    [(_ name [field:field ...+] #:final-deserialize final-deserialize)
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
           (final-deserialize new)
           new))]))

(defstruct file
  (;; The `arrows` field is created from a few of the other fields;
   ;; see add-arrows function, below. To save space, we serialize it
   ;; as #f. Upon deserialization we create an empty arrow-map, then
   ;; finally (after all other struct fields deserialized) call
   ;; add-arrows to populate it.
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
   [pdb-exports (make-hash)] ;(hash ibk? export?)
   [pdb-imports (make-hash)] ;(hash (seteq (or/c symbol? spec)))
   [pdb-import-renames (mutable-set) set->list list->mutable-set] ;(set list)
   [pdb-export-renames (mutable-set) set->list list->mutable-set] ;(set export-rename-arrow)
   [pdb-sub-ranges (make-hash)] ;(hash ibk? (list/c (vector/c offset span sub-sym sub-pos)))
   )
  #:final-deserialize file-add-arrows)

(define (list->mutable-set xs)
  (apply mutable-set xs))

(define (file-add-arrows f)
  (define am (file-arrows f))
  (define (add-syncheck-arrows)
    ;; Note that check-syntax will give us two arrows when a simple
    ;; prefix-in require clause expands to a prefix #%require clause.
    ;; More complicated prefix-in will expand to #%require rename
    ;; clauses with a syntax preoprty, handled elsewhere below.
    (for ([sa (in-set (file-syncheck-arrows f))])
      (match-define (syncheck-arrow def-beg def-end def-px def-py
                                    use-beg use-end use-px use-py
                                    actual? phase require-arrow
                                    use-stx-datum use-sym def-sym rb)
        sa)
      (cond
        [(and require-arrow
              ;; Treat use of prefix-in prefix as a lexical-arrow to
              ;; the prefix (instead of an import-arrow to the
              ;; modpath). FIXME: This test is very ad hoc. Looks for
              ;; name mismatch, but not zero-width items like #%app or
              ;; #%datum.
              (not
               (and (equal? (~a use-stx-datum)
                            (~a use-sym (resolved-binding-nom-sym rb)))
                    (< use-beg use-end))))
         (arrow-map-set! am
                         ((if (eq? require-arrow 'module-lang)
                              lang-import-arrow
                              import-arrow)
                          phase
                          use-beg
                          use-end
                          def-beg
                          def-end
                          use-sym
                          (cons (resolved-binding-from-path rb)
                                (ibk (resolved-binding-from-subs rb)
                                     (resolved-binding-from-phase rb)
                                     (resolved-binding-from-sym rb)))
                          (cons (resolved-binding-nom-path rb)
                                (ibk (resolved-binding-nom-subs rb)
                                     (resolved-binding-nom-export-phase+space rb)
                                     (resolved-binding-nom-sym rb)))))]
        [else
         (arrow-map-set! am
                         (lexical-arrow phase
                                        use-beg
                                        use-end
                                        def-beg
                                        def-end
                                        use-sym
                                        def-sym))])))

  (define (add-export-rename-arrows)
    (for ([a (in-set (file-pdb-export-renames f))])
      (arrow-map-set! am a)))

  (define (add-and-adjust-arrows-for-import-renames)
    (for ([v (in-set (file-pdb-import-renames f))])
      ;; Given surface syntax that expands to a `#%require` `rename`
      ;; clause, such as either:
      ;;
      ;;     (require (rename-in modpath [old new]))
      ;;     new
      ;;
      ;;  or
      ;;
      ;;     (require (prefix-in A: (prefix-in B: old)))
      ;;
      ;; where `new-stx` has srloc in the former case, but not in the
      ;; latter:
      ;;
      (match-define (list phase
                          old-sym old-beg old-end
                          new-sym new-beg new-end new-prefix-parts
                          modpath-beg modpath-end) v)
      (unless (or (and new-beg new-end)
                  new-prefix-parts)
        (log-pdb-warning "import rename: Expected new id either to have srcloc or prefix parts"))
      ;; 1. Add import-rename-arrow from new to old, if both have
      ;; srcloc.
      (when (and old-beg old-end new-beg new-end
                 (not (= old-beg new-beg))
                 (not (= old-end new-end)))
        (arrow-map-set! am
                        (import-rename-arrow phase
                                             new-beg
                                             new-end
                                             old-beg
                                             old-end
                                             old-sym
                                             new-sym)))
      ;; 2. Update any existing import-arrows pointing to the same
      ;; `modpath` and using new-sym (or parts thereof), instead to be
      ;; lexical arrows pointing to `new`.
      ;;
      ;; 3. Move original import arrow, so now it points from `old` to
      ;; `modpath`. Important we use original arrow here for its
      ;; import-arrow-from and import-arrow-nom field values.
      (for ([a (in-set (span-map-ref (arrow-map-def->uses am) modpath-beg (set)))])
        (when (and (import-arrow? a)
                   (equal? (import-arrow-sym a) new-sym))
          (match-define (cons _path (ibk _mods phase import-nom-sym)) (import-arrow-nom a))
          (match new-prefix-parts
            [(? list? subs)
             (for ([sub (in-list subs)])
               (match-define (vector offset span sub-sym sub-pos) sub)
               (cond
                 [(equal? sub-sym import-nom-sym)
                  ;; Adjust arrow-use-beg of original import-arrow.
                  ;;
                  ;; To do so, we need to remove from arrow-map
                  ;; original coords, then set it again at new
                  ;; coords.
                  (arrow-map-remove! am a)
                  (arrow-map-set! am
                                  (import-arrow (arrow-phase a)
                                                (+ (arrow-use-beg a) offset)
                                                (arrow-use-end a)
                                                (arrow-def-beg a)
                                                (arrow-def-end a)
                                                (import-arrow-sym a)
                                                (import-arrow-from a)
                                                (import-arrow-nom a)))]
                 [else
                  (arrow-map-set! am
                                  (lexical-arrow phase
                                                 (+ (arrow-use-beg a) offset)
                                                 (+ (arrow-use-beg a) offset span)
                                                 sub-pos
                                                 (+ sub-pos span)
                                                 sub-sym
                                                 sub-sym))]))]
            [#f
             (arrow-map-set! am
                             (lexical-arrow phase
                                            (arrow-use-beg a)
                                            (arrow-use-end a)
                                            new-beg
                                            new-end
                                            new-sym
                                            old-sym))
             (when (and (not (= old-beg modpath-beg))
                        (not (= old-end modpath-end)))
               (arrow-map-set! am
                               (import-arrow (arrow-phase a)
                                             old-beg
                                             old-end
                                             (arrow-def-beg a)
                                             (arrow-def-end a)
                                             (import-arrow-sym a)
                                             (import-arrow-from a)
                                             (import-arrow-nom a))))])))))
  (add-syncheck-arrows)
  (add-export-rename-arrows)
  (add-and-adjust-arrows-for-import-renames))
