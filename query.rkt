;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/contract
         racket/match
         (only-in "analyze.rkt" get-file)
         "data-types.rkt"
         "import-symbols.rkt"
         "span-map.rkt")

(provide get-annotations
         get-submodule-names
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
  (->* ((and/c path? complete-path?)) (position? position?) any) ;returns pdb?
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
       (match-define (syncheck-arrow def-beg def-end def-px def-py use-beg use-end use-px use-py actual? phase require-arrow _use-sym _def-sym _rb) a)
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

;; Private support function to get the (cons submodule-names
;; sees-enclosing?) value for a given point.
(define (get-submodule f pos)
  (define im (file-pdb-modules f))
  (match (interval-map-ref im pos #f)
    [(? pair? v) v]
    [#f
     ;; For files using "#lang", positions before the lang do not
     ;; correspond to any module. For those, just assume the first
     ;; module. Same for file using a (module __) form but with
     ;; leading whitespace. Deal with those, plus position past EOF,
     ;; by returning the first module.
     (define iter (dict-iterate-first im))
     (and iter (dict-iterate-value im iter))]))

;; Public API which returns just the list of submodule names.
(define (get-submodule-names path pos)
  (match (get-submodule (get-file path) pos)
    [(cons mods _sees-enclosing?) mods]
    [#f null]))

(module+ test
  (define-runtime-path modules.rkt (build-path "example" "modules.rkt"))
  (require "analyze.rkt")
  (analyze-path modules.rkt #:always? #t)
  (check-equal? (get-submodule-names modules.rkt 1)
                '())
  (check-equal? (get-submodule-names modules.rkt 52)
                '(m+))
  (check-equal? (get-submodule-names modules.rkt 122)
                '(m+))
  (check-equal? (get-submodule-names modules.rkt 201)
                '(m+))
  (check-equal? (get-submodule-names modules.rkt 250)
                '(m+))
  (check-equal? (get-submodule-names modules.rkt 253)
                '(m+))
  (check-equal? (get-submodule-names modules.rkt 267)
                '(m+ n+))
  (check-equal? (get-submodule-names modules.rkt 125)
                '(m))
  (check-equal? (get-submodule-names modules.rkt 149)
                '(m n))
  (check-equal? (get-submodule-names modules.rkt 175)
                '(m n o)))

;; Return list of completion candidates based on imports for the
;; module at `pos`. When the module can see its parents' bindings
;; (i.e. module+), also adds those.
(define (completion-candidates-from-imports f pos)
  (match (get-submodule f pos)
    ;; When there are no submodules, because file had errors, then
    ;; `analyze` will have copied the file-pdb-imports from the
    ;; previous successful analysis if any. To give the user
    ;; candidates while they are fixing the error, return union of all
    ;; imports from previous result. (This errs on the side of too
    ;; many, i.e. some might not be valid imported symbols. But it's
    ;; more useful than supplying none. Anyway AFAICT we can't know
    ;; what subset are valid, when a file has errors.)
    [#f (apply set-union (map resolve-import-set
                              (hash-values (file-pdb-imports f))))]
    [innermost
     (let loop ([s (seteq)]
                [v innermost])
       (match v
         [(cons mods #f)
          (set-union s (resolve-import-set
                        (hash-ref (file-pdb-imports f) mods (seteq))))]
         [(cons mods #t)
          (define enclosing-mods (reverse (cdr (reverse mods))))
          (loop (set-union s (resolve-import-set
                              (hash-ref (file-pdb-imports f) mods (seteq))))
                (for/or ([v (in-dict-values (file-pdb-modules f))])
                  (and (equal? enclosing-mods (car v)) v)))]
         [#f (seteq)]))]))

(module+ test
  (define f (get-file modules.rkt))
  (check-true (set-member? (completion-candidates-from-imports f 37)
                           'get-pure-port)
              "get-pure-port is a completion candidate in the file module, because it requires net/url")
  (check-true (set-member? (completion-candidates-from-imports f 109)
                           'get-pure-port)
              "get-pure-port is a completion candidate in the m+ submodule, because m+ is a module+ submodule of the file module")
  (check-false (set-member? (completion-candidates-from-imports f 149)
                            'get-pure-port)
               "get-pure-port is NOT a completion candidate in the m submodule")
  (check-true (set-member? (completion-candidates-from-imports f 283)
                           'get-pure-port)
              "get-pure-port is a completion candidate in the n+ submodule, because n+ is a module+ submodule of the m+ module+ submodule of the file module"))

;; Accepts a position to enable returning only /valid/ candidates
;; within a module or even lexical scope.
(define (get-completion-candidates path [pos 1])
  (define f (get-file path))
  (set-union
   (completion-candidates-from-imports f pos)
   (for/seteq ([v (in-hash-keys (file-syncheck-definition-targets f))])
     (ibk-sym v))
   ;; ~= to getting candidates from syncheck:add-mouse-over messages
   ;; about "bound occurrence(s)", which includes lexical arrows, plus
   ;; more from our rename-arrows. Note: Currently this does NOT try
   ;; to limit based on lexical scope; it errs on the side of
   ;; returning more candidates.
   (for*/fold ([s (seteq)])
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
  (define point-mouse-over
    (or (error-messages-here)
        (match (span-map-ref/bounds (file-syncheck-mouse-overs f) pos #f)
          [(cons (cons beg end) v) (list beg end v)]
          [#f #f])))
  ;; TODO: Filter use-sites that aren't within [beg end)? In the case
  ;; where there are very many use sites (hundreds or thousands?), it
  ;; could start to matter that we return so many that aren't visible.
  ;; OTOH if we do limit these to [beg end) here, we'd need to export
  ;; a new function to support front end {next previous}-use commands.
  (define point-def-and-use-sites
    (match (span-map-ref (arrow-map-use->def (file-arrows f)) pos #f)
      [(? arrow? u->d)
       (list (cons (arrow-def-beg u->d)
                   (arrow-def-end u->d))
             (import-arrow? u->d)
             (let ([d->us (span-map-ref (arrow-map-def->uses (file-arrows f))
                                        (arrow-def-beg u->d)
                                        (set))])
               (for/list ([d->u (in-set d->us)]
                          #:when (< (arrow-use-beg d->u)
                                    (arrow-use-end d->u)))
                 (cons (arrow-use-beg d->u)
                       (arrow-use-end d->u)))))]
      [_
       (match (span-map-ref (arrow-map-def->uses (file-arrows f)) pos (set))
         [(? set? d->us)
          #:when (not (set-empty? d->us))
          (list (cons (arrow-def-beg (set-first d->us))
                      (arrow-def-end (set-first d->us)))
                (import-arrow? (set-first d->us))
                (for/list ([d->u (in-set d->us)]
                           #:when (< (arrow-use-beg d->u)
                                     (arrow-use-end d->u)))
                  (cons (arrow-use-beg d->u)
                        (arrow-use-end d->u))))]
         [_ (list #f #f #f)])]))
  (define unused-requires
    (map car (span-map-refs (file-syncheck-unused-requires f) beg end)))
  ;; Although you might think unused bindings, which get a "no bound
  ;; occurrences" mouse-over, would be handled by the
  ;; 'unused-identifier text-type, that seems to be used only for
  ;; unused requires.
  ;;
  ;; Although you might think lexical arrows would be a good way to
  ;; find all definition sites, naturally there is no arrow drawn for
  ;; unused definitions (what would the other end be). So here, too,
  ;; the most reliable source of information, as hacky as it might be,
  ;; seems to be looking for mouse-overs with "bound occurence(s)".
  (define-values (def-sites unused-def-sites)
    (for/fold ([defs null]
               [unused null])
              ([v (in-list (span-map-refs (file-syncheck-mouse-overs f) beg end))])
      (match (for/or ([str (in-set (cdr v))])
               (cond [(equal? str "no bound occurrences")
                      (cons (car v) #t)]
                     [(regexp-match? #px"^\\d+ bound occurrences?$" str)
                      (cons (car v) #f)]
                     [else #f]))
        [(cons def unused?) (values (cons def defs) (if unused?
                                                        (cons def unused)
                                                        unused))]
        [#f (values defs unused)])))
  (define doc-sites
    (for/list ([v (in-list (span-map-refs (file-syncheck-text-types f) beg end))]
               #:when (eq? (cdr v) 'document-identifier))
      (car v)))
  (hash
   ;; This pertains only to point
   'point-mouse-over        point-mouse-over
   ;; This pertains to point and related sites, which may extend
   ;; beyond beg..end span.
   'point-def-and-use-sites point-def-and-use-sites
   ;; These pertain to entire beg..end span
   'def-sites               def-sites
   'unused-def-sites        unused-def-sites
   'unused-requires         unused-requires
   'doc-sites               doc-sites))

(module+ ex
  (require racket/path)
  (get-annotations (simple-form-path "example/define.rkt") 1500 1530)
  (get-annotations (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/typed-error.rkt"))
  (get-errors (simple-form-path "example/require-error.rkt"))
  (get-point-info (simple-form-path "example/define.rkt") 1353 1170 1536)
  (get-point-info (simple-form-path "example/define.rkt") 1 1 100))

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
