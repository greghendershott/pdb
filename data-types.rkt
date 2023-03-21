#lang racket/base

(require data/interval-map
         racket/dict
         racket/set
         "span-map.rkt")

(provide position?
         (struct-out ibk)
         (struct-out arrow)
         (struct-out lexical-arrow)
         (struct-out rename-arrow)
         (struct-out export-rename-arrow)
         (struct-out import-rename-arrow)
         (struct-out import-arrow)
         (struct-out arrow-map)
         arrow-map-set!
         arrow-map-arrows
         (struct-out file)
         new-file
         file-massage-before-serialize
         file-massage-after-deserialize)

;;; Data types

(define position? exact-positive-integer?)

;; identifier-binding uniquely refers to a non-lexical binding via a
;; tuple of <path mod phase symbol>. Often we need all but the path
;; since we're working on things per-file. A struct for that:
(struct ibk (mods phase sym) #:prefab)

;; An arrow always has both ends in the same file. (Arrows for
;; imported definitions point to e.g. the `require` or module language
;; syntax in the importing file. For such arrows, the derived struct
;; `import-arrow`, below, also says where to look outside the file.)
(struct arrow
  (phase
   use-beg
   use-end
   def-beg
   def-end)
  #:prefab)

(struct lexical-arrow arrow (sym) #:prefab)

(struct rename-arrow arrow (old-sym new-sym) #:prefab)
(struct export-rename-arrow rename-arrow () #:prefab)
(struct import-rename-arrow rename-arrow () #:prefab)

;; For syncheck:add-arrow require or module-lang arrows. `from` and
;; `nom` correspond to identifier-binding-resolved fields.
(struct import-arrow arrow
  (sym
   from ;(cons path? ibk?) used to look up in file's `defs` hash-table
   nom) ;(cons path? ibk?) used to look up in file's `exports hash-table
  #:prefab)

;; An arrow-map is a pair of span-maps, one for each "direction" of
;; def<->uses. (The same immmutable arrow struct instance is stored in
;; both; IIUC this is just a pointer, not two copies.)
(struct arrow-map
  (def->uses ;1:many (span-map def-beg def-end (set arrow))
   use->def) ;1:1    (span-map use-beg use-end arrow)
  #:prefab)

(define (make-arrow-map [as null])
  (define m (arrow-map (make-span-map) (make-span-map)))
  (for ([a (in-list as)])
    (arrow-map-set! m a))
  m)

(define (arrow-map-arrows am)
  (span-map-values (arrow-map-use->def am)))

(define (arrow-map-set! am a)
  (span-map-update*!/set (arrow-map-def->uses am)
                         (arrow-def-beg a)
                         (arrow-def-end a)
                         a)
  (span-map-set! (arrow-map-use->def am)
                 (arrow-use-beg a)
                 (arrow-use-end a)
                 a))

;; When changing fields here, also update `new-file` and the
;; `file-massage-xxx` functions, just below.
(struct file
  (digest            ;(or/c #f string?): sha1
   arrows            ;arrow-map?
   defs              ;(hash-table ibk? (cons def-beg def-end))
   exports           ;(hash-table ibk? (or/c (cons def-beg def-end) (cons path? ibk?))
   imports           ;(set/c symbol?)
   mouse-overs       ;(span-map beg end (set string?))
   tail-arrows       ;(set/c (list/c (or/c #f path?) integer? (or/c #f path?) integer?)
   docs              ;(span-map beg end (cons path-string anchor-string))
   unused-requires   ;(span-map be end #t
   sub-range-binders ;(hash-table ibk? (interval-map ofs-beg ofs-end (list def-beg def-end def-id)
   errors            ;(span-map beg end (set (cons (or/c #f path?) string?)))
   ) #:prefab)

(define (new-file [digest #f])
  (file digest
        (make-arrow-map)  ;arrows
        (make-hash)       ;defs
        (make-hash)       ;exports
        (mutable-set)     ;imports
        (make-span-map)   ;mouse-overs
        (mutable-set)     ;tail-arrows
        (make-span-map)   ;docs
        (make-span-map)   ;unused-requires
        (make-hash)       ;sub-range-binders
        (make-span-map))) ;errors

;; Massage data to/from the subset that racket/serialize requires.
;; Includes details like making sure that on load we have mutable
;; hash-tables when the default might be immutable.
(define (file-massage-before-serialize f)
  (struct-copy
   file f
   [arrows            (arrow-map-arrows (file-arrows f))]
   [imports           (set->list (file-imports f))]
   [tail-arrows       (set->list (file-tail-arrows f))]
   [unused-requires   (span-map->list (file-unused-requires f))]
   [mouse-overs       (span-map->list (file-mouse-overs f))]
   [docs              (span-map->list (file-docs f))]
   [sub-range-binders (for/hash ([(k v) (in-hash (file-sub-range-binders f))])
                        (values k (dict->list v)))]
   [errors            (span-map->list (file-errors f))]))

(define (file-massage-after-deserialize f)
  (struct-copy
   file f
   [arrows            (make-arrow-map (file-arrows f))]
   [imports           (apply mutable-set (file-imports f))]
   [tail-arrows       (apply mutable-set (file-tail-arrows f))]
   [unused-requires   (apply make-span-map (file-unused-requires f))]
   [mouse-overs       (apply make-span-map (file-mouse-overs f))]
   [docs              (apply make-span-map (file-docs f))]
   [sub-range-binders (make-hash ;mutable
                       (for/list ([(k v) (in-hash (file-sub-range-binders f))])
                         (cons k (make-interval-map v))))]
   [errors            (apply make-span-map (file-errors f))]))
