#lang racket/base

(require racket/match
         "common.rkt"
         "data-types.rkt"
         "span-map.rkt")
(provide file-add-arrows)

;; Given a set of syncheck-arrows, create our own set of arrows that
;; enable smart renaming, as used by relations.rkt.
;;
;; Note: Much of this could go away if check-syntax used the new
;; import-or-export-prefix-ranges syntax property to supply more
;; prefix arrows in the first place. I plan to see if I can make such
;; a change to traversals.rkt myself, or at least talk to Robby about
;; him doing it.
;;
;; Another part of it is that we add an "import-rename-arrow" where we
;; detect surface syntax like rename-in or only-in. In such cases the
;; new name site is of interest for rename commands. Although that
;; seems less obviously something to add to traversals.rkt, it might
;; be.

(define (file-add-arrows f)
  (define am (file-arrows f))
  (for ([sa (in-set (file-syncheck-arrows f))])
    (match-define (syncheck-arrow def-beg def-end _def-px _def-py
                                  use-beg use-end _use-px _use-py
                                  _actual? phase require-arrow
                                  use-sym def-sym rb)
      sa)
    (cond
      [(not require-arrow)
       (arrow-map-set! am (lexical-arrow phase
                                         use-beg use-end
                                         def-beg def-end
                                         use-sym def-sym))]
      [;; When a simple (require (prefix-in)) expands to (#%require
       ;; [prefix]) then check-syntax handles this and gives us two
       ;; arrows. That's fine, we just prefer to classify the prefix
       ;; arrow as a lexical-arrow instead of an import-arrow.
       (span-map-ref (file-syncheck-prefixed-requires f) def-beg #f)
       (arrow-map-set! am (lexical-arrow phase
                                         use-beg use-end
                                         def-beg def-end
                                         use-sym def-sym))]
      [else ;other require-arrow
       ;; Our base case import-arrow corresponding to the syncheck
       ;; require arrow. We might simply insert this as-is. However if
       ;; this import is involved with a #%require rename clause, we
       ;; might adjust some elements of the import arrow before
       ;; inserting, as well as insert additional, lexical arrows.
       (define ia ((if (eq? require-arrow 'module-lang)
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
                              (resolved-binding-nom-sym rb)))))

       ;; Is there a #%require rename clause associated with this,
       ;; i.e. its modpath loc matches this def loc, and its new
       ;; symbol matches this use symbol?
       (match (hash-ref (file-pdb-import-renames f)
                        (list def-beg def-end use-sym)
                        #f)
         [(import-rename phase
                         old-sym old-beg old-end
                         new-sym new-beg new-end new-prefix-parts
                         modpath-beg modpath-end)
          ;; Although we're given information about a fully-expanded
          ;; #%require rename clause, that can result from a
          ;; surprising variety of things -- including but definitely
          ;; _not_ limited to a surface require clause like rename-in
          ;; or only-in where both the old and new names are present
          ;; in the original source. This predicate is a somewhat ad
          ;; hoc way to determine if some such simple surface rename
          ;; is involved here. (Some more complicated renames are
          ;; handled properly by the import-or-export-prefix-ranges
          ;; property, the essence of which in `prefix-parts`.)
          ;;
          ;; This predicate excludes rename clauses that don't
          ;; actually rename (as can happen with multi-in); that lack
          ;; srloc; and if they do have srloc, the old/new/modpath are
          ;; all the same loc. (An even stronger test, which we don't
          ;; do yet, would be to compare the syntax-e to the source
          ;; text for the given position and span. This would guard
          ;; against macros doing various bizarre things, including
          ;; forgetting that srcloc is _supposed_ to mean a location
          ;; in the _original source_. Syntax present only in
          ;; fully-expanded code _should_ have false srcloc.)
          (define (surface-rename?)
            (and (not (eq? old-sym new-sym))
                 old-beg old-end new-beg new-end
                 (not (= old-beg new-beg modpath-beg))
                 (not (= old-end new-end modpath-end))))
          (when (surface-rename?)
            (arrow-map-set! am
                            (import-rename-arrow phase
                                                 new-beg new-end
                                                 old-beg old-end
                                                 old-sym new-sym)))
          (match new-prefix-parts
            [(? list? subs)
             ;; Insert multiple arrows, one for each piece.
             (for ([sub (in-list subs)])
               (match-define (sub-range offset span sub-sym sub-pos) sub)
               (cond
                 [(equal? sub-sym (resolved-binding-nom-sym rb))
                  ;; Adjust arrow-use-beg of base import-arrow.
                  (arrow-map-set! am
                                  (import-arrow (arrow-phase ia)
                                                (+ (arrow-use-beg ia) offset)
                                                (arrow-use-end ia)
                                                (arrow-def-beg ia)
                                                (arrow-def-end ia)
                                                (import-arrow-sym ia)
                                                (import-arrow-from ia)
                                                (import-arrow-nom ia)))]
                 [else
                  (arrow-map-set! am
                                  (lexical-arrow phase
                                                 (+ (arrow-use-beg ia) offset)
                                                 (+ (arrow-use-beg ia) offset span)
                                                 sub-pos
                                                 (+ sub-pos span)
                                                 sub-sym
                                                 sub-sym))]))]
            [#f
             (cond
               [(surface-rename?)
                (arrow-map-set! am
                                (lexical-arrow phase
                                               (arrow-use-beg ia)
                                               (arrow-use-end ia)
                                               new-beg
                                               new-end
                                               new-sym
                                               old-sym))
                (arrow-map-set! am
                                (import-arrow (arrow-phase ia)
                                              old-beg
                                              old-end
                                              (arrow-def-beg ia)
                                              (arrow-def-end ia)
                                              (import-arrow-sym ia)
                                              (import-arrow-from ia)
                                              (import-arrow-nom ia)))]
               [else (arrow-map-set! am ia)])])]
         [#f (arrow-map-set! am ia)])]))
  ;; Add arrows for certain #%provide rename clauses.
  (for ([a (in-set (file-pdb-export-renames f))])
    (arrow-map-set! am a)))

(current-file-add-arrows file-add-arrows)
