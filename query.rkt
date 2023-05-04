;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/match
         (only-in "analyze.rkt" get-file)
         "data-types.rkt"
         "span-map.rkt")

(provide get-annotations
         get-completion-candidates
         get-errors
         get-point-info
         get-doc-link
         get-require-path)

(module+ test
  (require (for-syntax racket/base)
           rackunit
           racket/path
           racket/runtime-path))

;;; Simple queries

;; Most annotations pertain to specific spans. There are various
;; kinds. get-annotations returns most mixed and sorted by position.
;; (See get-errors and get-completion-candidates for two things that
;; get-annotations does /not/ return.)
;;
;; 1. This query supports the sort of access pattern that Racket Mode's
;; "classic" racket-xp-mode uses: Get values for everything and put as
;; text properties into the buffer.
;;
;; That access pattern is not great for large files with a lot of
;; annotation data. It takes space on the front end client (e.g.
;; emacs/vim/vscode). Just as bad is marshaling overhead (converting
;; to/from json or sexp or whatever format is used for the Racket back
;; end to talk to the non-Racket front end).
;;
;; 2. This query also supports getting only a subset for a certain
;; span. This supports better access patterns. See also
;; `get-point-info`, below, which is especially optimized for one such
;; access pattern.
;;
;; TODO: Now that we record zero-span items, notably for things like
;; #%app and #%datum, we should add a flag here to ignore these. Some
;; clients -- certainly Emacs -- can't use these, and they are
;; numerous, so in such cases best not to marshal them at all.
(define/contract (get-annotations path [beg 1] [end max-position])
  (->* (complete-path?) (position? position?) any) ;returns pdb?
  (define f (get-file path))
  (define (arrows)
    ;; FIXME: Iterating entire set is slow; consider storing
    ;; syncheck-arrows in a pair of span-maps (something like our
    ;; arrow-map but for syncheck-arrows).
    (set->list
     (for*/set ([a (in-set (file-syncheck-arrows f))]
                #:when (or (and (<= beg (syncheck-arrow-def-beg a))
                                (< (syncheck-arrow-def-end a) end))
                           (and (<= beg (syncheck-arrow-use-beg a))
                                (< (syncheck-arrow-use-end a) end))))
       (match-define (syncheck-arrow def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow _use-stx-datum _use-sym _def-sym _rb) a)
       (list 'arrow def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow))))
  (define (mouse-overs)
    (for/list ([v (in-list (span-map-refs (file-syncheck-mouse-overs f) beg end))])
      (match-define (cons (cons beg end) texts) v)
      (list 'mouse-over beg end texts)))
  (define (doc-sites)
    (for/list ([v (in-list (span-map-refs (file-syncheck-docs-menus f) beg end))])
      (match-define (cons (cons beg end) d) v)
      (list 'doc-link beg end (syncheck-docs-menu-path d) (syncheck-docs-menu-anchor-text d))))
  (define (unused-requires)
    (for/list ([v (in-list (span-map-refs (file-syncheck-unused-requires f) beg end))])
      (match-define (cons (cons beg end) _) v)
      (list 'unused-require beg end)))
  (define (require-opens)
    (for/list ([v (in-list (span-map-refs (file-syncheck-require-opens f) beg end))])
      (match-define (cons (cons beg end) path) v)
      (list 'require beg end path)))
  (define (text-types)
    (for/list ([v (in-list (span-map-refs (file-syncheck-text-types f) beg end))])
      (match-define (cons (cons beg end) type) v)
      (list 'type beg end type)))
  (sort (append (arrows)
                (mouse-overs)
                (doc-sites)
                (require-opens)
                (unused-requires)
                (text-types))
        < #:key cadr))

;; Optionally accepts a position with the view that someday we'd build
;; a more-targeted data structure for this -- limited to /valid/
;; candidates within a module or even lexical scope. But for now we
;; continue the Racket Mode tradition of erring on the side of
;; offering more candidates, even if some aren't valid.
(define (get-completion-candidates path [_pos 1])
  (define f (get-file path))
  (set-union
   (for/set ([v (in-set (file-pdb-imports f))]) ;-> immutable-set
     v)
   ;; ~= to getting candidates from syncheck:add-definition-target.
   (for/set ([v (in-hash-keys (file-syncheck-definition-targets f))])
     (ibk-sym v))
   ;; ~= to getting candidates from synchek:add-mouse-over messages
   ;; about "bound occurrence(s)", which includes lexical arrows, plus
   ;; more from our rename-arrows.
   (for*/fold ([s (set)])
              ([uses (in-list (span-map-values (arrow-map-def->uses (file-arrows f))))]
               [use (in-set uses)])
     (match use
       [(? lexical-arrow? a)
        (set-add s (lexical-arrow-use-sym a))]
       [(? rename-arrow? a)
        (set-add (set-add s (rename-arrow-old-sym a))
                 (rename-arrow-new-sym a))]
       [_ s]))))

;; Accepts no span or position. Justification:
;;
;; 0. There are unlikely to be very many. Most expansion errors result
;;    in a single exn error message. Even things like Typed Racket
;;    that call error-display-handler multiple times before rasing a
;;    single exn, tend not to have more than (say) a dozen.
;;
;; 1. A user will want to see/visit all the error locations,
;;    regardless of where they might be in the file.
;;
;; 2. The errors returned for `path` might be in another, imported
;;    file, for which any span or position or span in `path` is N/A.
(define (get-errors path)
  (for/list ([v (in-list (span-map->list (file-pdb-errors (get-file path))))])
    (match-define (list (cons beg end) (cons maybe-path message)) v)
    (list beg end
          (or maybe-path (path->string path))
          message)))

;; This is designed for a client that does not want to store any
;; persistent values on its end. For example, an Emacs mode that does
;; not store every annotation as a text property. Instead, upon
;; movement of window-point or window-{start end} (to use Emacs
;; terminology), it can call this to get only values pertaining to
;; that subset of the buffer. Presumably it can temporarily enhance
;; the presentation (e.g. add overlays in Emacs).
;;
;; In principle a client could write this itself by filtering
;; information from `get-annotations` Maybe this shouldn't even exist
;; as part of library, but just be example code? Anyway it's here for
;; now as I dog-food the use of pdb by Racket Mode for Emacs, and
;; learn more from some use in the real world.
(define (get-point-info path pos beg end)
  (define f (get-file path))
  (define (error-messages-here)
    (match (span-map-ref/bounds (file-pdb-errors f) pos #f)
      [(cons (cons beg end) a-set)
       (and (not (set-empty? a-set))
            (list beg end
                  (for*/set ([v (in-set a-set)]
                             [err-path (in-value (car v))]
                             [err-msg  (in-value (cdr v))]
                             #:when (or (not err-path)
                                        (equal? err-path (path->string path))))
                    err-msg)))]
      [#f #f]))
  ;; TODO: Should we return all mouse-overs for [beg end), in case the
  ;; client wants to support actual GUI tooltips? In that case if the
  ;; client wants to treat a mouse-over at point specially (e.g.
  ;; racket-show in Racket Mode), let it distinguish that itself?
  (define mouse-over
    (or (error-messages-here)
        (match (span-map-ref/bounds (file-syncheck-mouse-overs f) pos #f)
          [(cons (cons beg end) v) (list beg end v)]
          [#f #f])))
  ;; TODO: Filter use-sites that aren't within [beg end)? In the case
  ;; where there are very many use sites (hundreds or thousands?), it
  ;; could start to matter that we return so many that aren't visible.
  (define-values (def-site use-sites)
    (match (span-map-ref (arrow-map-use->def (file-arrows f)) pos #f)
      [(? arrow? u->d)
       #:when (not (import-arrow? u->d))
       (values (cons (arrow-def-beg u->d)
                     (arrow-def-end u->d))
               (for/list ([d->u (in-set (span-map-ref (arrow-map-def->uses (file-arrows f))
                                                      (arrow-def-beg u->d)
                                                      (set)))])
                 (cons (arrow-use-beg d->u)
                       (arrow-use-end d->u))))]
      [_
       (match (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set))
         [(? set? d->us)
          #:when (not (set-empty? d->us))
          (values (cons (arrow-def-beg (set-first d->us))
                        (arrow-def-end (set-first d->us)))
                  (for/list ([d->u (in-set d->us)]
                             #:when (not (import-arrow? d->u)))
                    (cons (arrow-use-beg d->u)
                          (arrow-use-end d->u))))]
         [_ (values #f #f)])]))
  (define unused-requires
    (map car (span-map-refs (file-syncheck-unused-requires f) beg end)))
  (define unused-bindings
    (for/list ([v (in-list (span-map-refs (file-syncheck-mouse-overs f) beg end))]
               #:when (set-member? (cdr v) "no bound occurrences"))
      (car v)))
  (hash
   ;; This pertains only to point
   'mouse-over      mouse-over
   ;; These pertain to point and related sites
   'def-site        def-site
   'use-sites       use-sites
   ;; These pertain to entire beg..end span
   'unused-requires unused-requires
   'unused-bindings unused-bindings))

(module+ ex
  (require racket/path)
  (get-annotations (simple-form-path "example/define.rkt") 1500 1530)
  (get-annotations (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/require-error.rkt"))
  #;(get-completion-candidates (simple-form-path (build-path "example" "define.rkt")))
  (get-point-info (simple-form-path "example/define.rkt") 1353 1170 1536))

(define (get-doc-link path pos)
  (define d (span-map-ref (file-syncheck-docs-menus (get-file path))
                          pos
                          #:try-zero-width? #t
                          #f))
  (and d (cons (syncheck-docs-menu-path d) (syncheck-docs-menu-anchor-text d))))

(module+ test
  (define-runtime-path typed.rkt (build-path "example" "typed.rkt"))
  (define (convert v) ;full doc paths not portable for tests
    (match v
      [(cons p a) (cons (file-name-from-path p) a)]
      [_ #f]))
  (check-equal? (convert (get-doc-link typed.rkt 54))
                (cons (build-path "generic-numbers.html")
                      "(def._((quote._~23~25kernel)._+))")
                "get-doc-linked returns expected file and anchor")
  (check-false (convert (get-doc-link typed.rkt 25))
               "get-doc-linked returns false when no doc exists")
  (check-equal? (convert (get-doc-link typed.rkt 53))
                (cons (build-path "application.html")
                      "(form._((lib._racket/private/base..rkt)._~23~25app))")
                "get-doc-link finds docs for zero-width-items as a fallback")
  (check-equal? (convert (get-doc-link typed.rkt 58))
                (cons (build-path "quote.html")
                      "(form._((quote._~23~25kernel)._~23~25datum))")
                "get-doc-link finds docs for zero-width-items as a fallback"))

(define (get-require-path path pos)
  (span-map-ref (file-syncheck-require-opens (get-file path)) pos #f))

(module+ test
  (define-runtime-path require.rkt (build-path "example" "require.rkt"))
  (require syntax/modresolve)
  (check-false (get-require-path require.rkt 1))
  (define-runtime-path define.rkt (build-path "example" "define.rkt"))
  (check-equal? (get-require-path require.rkt 28) define.rkt)
  (define base.rkt (resolve-module-path 'racket/base))
  (check-equal? (get-require-path require.rkt 7) base.rkt))
