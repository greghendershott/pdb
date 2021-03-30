#lang racket/base

(require drracket/check-syntax
         racket/class
         racket/format
         racket/match
         racket/path
         racket/set
         syntax/modread
         (prefix-in db: "db.rkt")
         "contract-hack.rkt")

(provide analyze-code)

(define (analyze-code path code-str)
  (string->syntax
   path
   code-str
   (λ (stx)
     (parameterize ([current-namespace (make-base-namespace)])
       (define exp-stx (expand stx))
       (analyze-using-check-syntax path exp-stx code-str)
       (maybe-use-hack-for-contract-wrappers db:add-def path exp-stx)))))

(define (string->syntax path code-str [k values])
  (define dir (path-only path))
  (parameterize ([current-load-relative-directory dir]
                 [current-directory               dir])
    (k
     (with-module-reading-parameterization
       (λ ()
         (define in (open-input-string code-str path))
         (port-count-lines! in)
         (match (read-syntax path in)
           [(? eof-object?) #'""]
           [(? syntax? stx) stx]))))))

;;; analyze: using check-syntax

;; Note: drracket/check-syntax reports things as zero-based [from upto)
;; but we handle them as one-based [from upto).

(define annotations-collector%
  (class (annotations-mixin object%)
    (init-field src code-str)

    (define more-files (mutable-set))
    (define (add-file-to-analyze file)
      (set-add! more-files file))
    (define/public (notify-more-files-to-analyze)
      (db:queue-more-files-to-analyze more-files))

    ;; For speed, an in-memory hash to avoid needing to look up
    ;; submods in syncheck:add-arrow/name-dup/pxpy.
    (define ht-defs (make-hash))
    (define (add-def-submods sym beg end mods)
      (hash-set! ht-defs (list sym beg end) mods))
    (define (get-def-submods sym beg end)
      (hash-ref ht-defs (list sym beg end) #f))

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-definition-target _ beg* end* symbol rev-mods)
      (define beg (add1 beg*))
      (define end (add1 end*))
      (define submods (reverse rev-mods))
      (add-def-submods symbol beg end submods)
      (db:add-def src beg end submods symbol))

    (define/override (syncheck:add-jump-to-definition _ beg end sym path submods)
      (when (file-exists? path)
        (define ofs (rename-offset (substring code-str beg end)
                                   (symbol->string sym)))
        (db:add-use src (add1 beg) (add1 end) path submods sym ofs)
        (add-file-to-analyze path)))

    ;; Handling this lets us also record uses within this source file
    ;; (which aren't reported by syncheck:add-jump-to-definition) of
    ;; definitions reported by syncheck:add-definition-target.
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      _def-src def-beg def-end _def-px _def-py
                      _use-src use-beg use-end _use-px _use-py
                      _actual? _level require-arrow? _name-dup?)
      (unless require-arrow? ;unless covered by add-jump-to-definition
        ;; This doesn't state the submods for the definition. Try to
        ;; look that up for a definition target we already recorded at
        ;; this location. If that lookup fails, then ignore this
        ;; (which is probably fine because this is probably an arrow
        ;; pointing to a lexical variable.)
        (define sym (string->symbol (substring code-str def-beg def-end)))
        (define submods (get-def-submods sym (add1 def-beg) (add1 def-end)))
        (when submods
          (db:add-use src (add1 use-beg) (add1 use-end) src submods sym 0))))

    (define/override (syncheck:add-require-open-menu _ _beg _end file)
      (add-file-to-analyze file))

    (super-new)))

(define (analyze-using-check-syntax path exp-stx code-str)
  (parameterize ([current-annotations (new annotations-collector%
                                           [src path]
                                           [code-str code-str])])
    (define-values (expanded-expression expansion-completed)
      (make-traversal (current-namespace)
                      (current-load-relative-directory)))
    (expanded-expression exp-stx)
    (expansion-completed)
    (send (current-annotations) notify-more-files-to-analyze)))

;;; renaming

;; This is a hack to determine whether an add-jump-to-definition is a
;; use that could/should be automatically renamed. It allows for:
;;
;; 1. The imported identifier's true name being of the form
;;    `provide/contract-id-XXX.N`, which is the case for the wrapper
;;    functions exported by `contract-out`. If the source identifer is XXX,
;;    then it is rename-able in the source.
;;
;; 2. The use identifier having a prefix (e.g. from `prefix-in`), but
;;    thereafter matching the referenced identifier -- as possibly
;;    adjusted by 1.
;;
;; It returns -1 if it thinks an automatic renaming can't be done.
;;
;; Otherwise it returns an offset into the `beg` value reported by
;; add-jump-to-definition where the rename-able portion is; i.e. 0
;; for when no prefix, else the index past the prefix.
;;
;; Why is this a hack: A "correct" approach wouldn't hardcode
;; knowledge of contract-out wraper names. A correct approach might
;; try to analyze require forms directly or make use of
;; syncheck:add-require-prefix plus syncheck:add-arrow (although I
;; think the prefix heuristic here doesn't have false positives??).
;;
;; Why is this here: All of this could be done by a front-end tool
;; that queries for a full set of uses of a definition, then prunes
;; the list. The justification for doing it here and storing in the
;; db, is that the query results could be smaller, and the processing
;; thereof more automatic. Anyway, it is one extra integer column in
;; the db, and a user could ignore this to do their own strategy if
;; they prefer.

(define (rename-offset full-use-site-str full-def-site-str)
  (define def-site-str
    (match full-def-site-str
      [(pregexp "^provide/contract-id-(.+)[.]\\d" (list _ v)) v]
      [_ full-def-site-str]))
  (match full-use-site-str
    [(pregexp (~a "^(.*)" (regexp-quote def-site-str) "$")
              (list _ prefix))
     (string-length prefix)]
    [_ -1]))

(module+ test
  (require racket/format
           rackunit
           syntax/parse/define)
  (define-simple-macro (chk src import ofs)
    (check-equal? (rename-offset src import)
                  ofs
                  (~a `(src import))))
  (chk "abc"       "def"                      -1)
  (chk "ID"        "ID"                        0)
  (chk "ID"        "provide/contract-id-ID.0"  0)
  (chk "ID"        "provide/contract-id-ID.1"  0)
  (chk "prefix:ID" "ID"                        7)
  (chk "prefix:ID" "provide/contract-id-ID.1"  7))
