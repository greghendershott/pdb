#lang racket/base

(require data/interval-map
         racket/dict
         racket/set)

(provide position?
         (struct-out ibk)
         (struct-out arrow)
         (struct-out lexical-arrow)
         (struct-out rename-arrow)
         (struct-out export-rename-arrow)
         (struct-out import-rename-arrow)
         (struct-out import-arrow)
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
   ;; We don't store `use-beg` or `use-end` here because we assume
   ;; this struct will be the value in an interval-map keyed by those,
   ;; and we don't redundantly store them here.
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
   from ;(cons path? key?) used to look up in file's `defs` hash-table
   nom  ;(cons path? key?) used to look up in file's `exports hash-table
   ) #:prefab)

;; When changing fields here, also update `new-file` and the
;; `file-massage-xxx` functions, just below. Also see db.rkt
;; for columns.
(struct file
  (digest            ;(or/c #f string?): sha1
   arrows            ;(interval-map use-beg use-end arrow?)
   defs              ;(hash-table ibk? (cons def-beg def-end))
   exports           ;(hash-table ibk? (or/c (cons def-beg def-end) (cons path? ibk?))
   imports           ;(set symbol?)
   mouse-overs       ;(interval-map beg end string?)
   tail-arrows       ;(set (cons integer? integer?)
   unused-requires   ;(set (cons beg end)
   sub-range-binders ;(hash-table key? (interval-map ofs-beg ofs-end (list def-beg def-end def-id)
   errors            ;(set (list (or/c #f path?) position? position? string?))
   ) #:prefab)

(define (new-file [digest #f])
  (file digest
        (make-interval-map)
        (make-hash)
        (make-hash)
        (mutable-set)
        (make-interval-map)
        (mutable-set)
        (mutable-set)
        (make-hash)
        (mutable-set)))

;; Massage data to/from the subset that racket/serialize requires.
;; Includes details like making sure that on load we have mutable
;; hash-tables when the default might be immutable.
(define (file-massage-before-serialize f)
  (struct-copy
   file f
   [arrows            (dict->list (file-arrows f))]
   [imports           (set->list (file-imports f))]
   [tail-arrows       (set->list (file-tail-arrows f))]
   [unused-requires   (set->list (file-unused-requires f))]
   [mouse-overs       (dict->list (file-mouse-overs f))]
   [sub-range-binders (for/hash ([(k v) (in-hash (file-sub-range-binders f))])
                        (values k (dict->list v)))]
   [errors            (set->list (file-errors f))]))

(define (file-massage-after-deserialize f)
  (struct-copy
   file f
   [arrows            (make-interval-map (file-arrows f))]
   [imports           (apply mutable-set (file-imports f))]
   [tail-arrows       (apply mutable-set (file-tail-arrows f))]
   [unused-requires   (apply mutable-set (file-unused-requires f))]
   [mouse-overs       (make-interval-map (file-mouse-overs f))]
   [sub-range-binders (make-hash ;mutable
                       (for/list ([(k v) (in-hash (file-sub-range-binders f))])
                         (cons k (make-interval-map v))))]
   [errors            (apply mutable-set (file-errors f))]))
