;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require data/interval-map
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

         make-arrow-map
         (struct-out arrow-map)
         arrow-map-set!
         arrow-map-arrows

         (struct-out syncheck-docs-menu)
         (struct-out syncheck-arrow)
         (struct-out syncheck-jump)
         (struct-out syncheck-prefixed-require-reference)
         (struct-out file)
         make-file
         file-massage-before-serialize
         file-massage-after-deserialize

         (struct-out file)
         add-arrows)

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

(struct syncheck-docs-menu (sym label path anchor anchor-text) #:prefab)
(struct syncheck-arrow (def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow use-stx-datum use-sym def-sym rb) #:prefab)
(struct syncheck-jump (sym path mods phase) #:prefab)
(struct syncheck-prefixed-require-reference (prefix prefix-beg prefix-end) #:prefab)

(struct file
  (;; The `arrows` field is created from a few of the other fields;
   ;; see add-arrows function, below. To save space, we serialize it
   ;; as #f, then create an arrow-map upon deserialization.
   arrows

   ;; From check-syntax. Effectively "just record the method calls".
   syncheck-arrows             ;(set/c syncheck-arrow?)
   syncheck-definition-targets ;(hash-table ibk? (cons def-beg def-end))
   syncheck-tail-arrows        ;(set/c (list/c (or/c #f path?) integer? (or/c #f path?) integer?)
   syncheck-jumps              ;span-map
   syncheck-prefixed-requires  ;span-map
   syncheck-mouse-overs        ;span-map; also items from our expansion
   syncheck-docs-menus         ;span-map
   syncheck-unused-requires    ;span-map
   syncheck-require-opens      ;span-map
   syncheck-text-types         ;spna-map

   ;; From our expansion
   pdb-errors

   ;; From our extra, `analyze-more` pass
   pdb-exports                 ;(hash-table ibk? (or/c (cons def-beg def-end) (cons path? ibk?))
   pdb-imports                 ;(set/c symbol?)
   pdb-import-renames          ;list
   pdb-export-renames          ;(mutable-set export-rename-arrow)
   pdb-sub-range-binders       ;(hash-table ibk? (interval-map ofs-beg ofs-end (list def-beg def-end def-id)
   ) #:prefab)

(define (make-file)
  (file (make-arrow-map)  ;arrows
        ;; syncheck-xxx
        (mutable-set)     ;syncheck-arrows
        (make-hash)       ;syncheck-definition-targets
        (mutable-set)     ;syncheck-tail-arrows
        (make-span-map)   ;syncheck-jumps
        (make-span-map)   ;syncheck-prefixed-requires
        (make-span-map)   ;syncheck-mouse-overs
        (make-span-map)   ;syncheck-docs-menus
        (make-span-map)   ;syncheck-unused-requires
        (make-span-map)   ;syncheck-require-opens
        (make-span-map)   ;syncheck-text-types
        ;; pdb-xxx
        (make-span-map)   ;pdb-errors
        (make-hash)       ;pdb-exports
        (mutable-set)     ;pdb-imports
        (mutable-set)     ;pdb-import-renames
        (mutable-set)     ;pdb-export-renames
        (make-hash)       ;pdb-sub-range-binders
        ))

;; Massage data to/from the subset that racket/serialize requires.
;; Includes details like making sure that on load we have mutable
;; hash-tables when the default might be immutable.
(define (file-massage-before-serialize f)
  (struct-copy
   file f
   [arrows                #f]
   [syncheck-arrows       (set->list (file-syncheck-arrows f))]
   [syncheck-tail-arrows  (set->list (file-syncheck-tail-arrows f))]
   [pdb-imports           (set->list (file-pdb-imports f))]
   [pdb-import-renames    (set->list (file-pdb-import-renames f))]
   [pdb-export-renames    (set->list (file-pdb-export-renames f))]
   [pdb-sub-range-binders (for/hash ([(k v) (in-hash (file-pdb-sub-range-binders f))])
                            (values k (dict->list v)))]))

(define (file-massage-after-deserialize f)
  (define new-f
    (struct-copy
     file f
     [arrows                (make-arrow-map)]
     [syncheck-arrows       (apply mutable-set (file-syncheck-arrows f))]
     [syncheck-tail-arrows  (apply mutable-set (file-syncheck-tail-arrows f))]
     [pdb-imports           (apply mutable-set (file-pdb-imports f))]
     [pdb-import-renames    (apply mutable-set (file-pdb-import-renames f))]
     [pdb-export-renames    (apply mutable-set (file-pdb-export-renames f))]
     [pdb-sub-range-binders (make-hash ;mutable
                             (for/list ([(k v) (in-hash (file-pdb-sub-range-binders f))])
                               (cons k (make-interval-map v))))]))
  (add-arrows new-f)
  new-f)

(define (add-arrows f)
  (define am (file-arrows f))
  (define (add-syncheck-arrows)
    ;; Note that check-syntax will give us two arrows for prefix-in
    ;; vars.
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
      (match-define (list phase
                          old-sym old-beg old-end
                          new-sym new-beg new-end
                          modpath-beg modpath-end) v)
      ;; Given
      ;;
      ;;     (require (rename-in modpath [old new]))
      ;;     new
      ;;
      ;; or same with `only-in`:
      ;;
      ;; 1. Add import-rename-arrow from new to old.
      (when (and old-beg old-end new-beg new-end
                 (not (equal? old-beg new-beg))
                 (not (equal? old-end new-end)))
        (arrow-map-set! am
                        (import-rename-arrow phase
                                             new-beg
                                             new-end
                                             old-beg
                                             old-end
                                             old-sym
                                             new-sym)))
      ;; 2. Update any existing import-arrows pointing to the same
      ;; `modpath` and using new-sym, instead to be lexical arrows
      ;; pointing to `new`.
      (when (and old-beg old-end new-beg new-end
                 modpath-beg modpath-end
                 (not (= new-beg modpath-beg))
                 (not (= new-end modpath-end)))
        (for ([a (in-set (span-map-ref (arrow-map-def->uses am) modpath-beg (set)))])
          (when (and (import-arrow? a)
                     (equal? (import-arrow-sym a) new-sym))
            (arrow-map-set! am
                            (lexical-arrow phase
                                           (arrow-use-beg a)
                                           (arrow-use-end a)
                                           new-beg
                                           new-end
                                           new-sym
                                           old-sym))
            ;; 3. Move original import arrow, so now it points from
            ;; `old` to `modpath`. Important we use original arrow
            ;; here for its import-arrow-from and import-arrow-nom
            ;; field values.
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
                                            (import-arrow-nom a)))))))))
  (add-syncheck-arrows)
  (add-export-rename-arrows)
  (add-and-adjust-arrows-for-import-renames))
