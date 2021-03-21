#lang racket/base

(require drracket/check-syntax
         racket/class
         racket/match
         racket/path
         racket/set
         syntax/modread
         "db.rkt"
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
       (maybe-use-hack-for-contract-wrappers add-def path exp-stx)))))

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
    (define/public (get-more-files-to-analyze)
      (set->list more-files))

    ;; For speed, an in-memory hash to avoid needing to look up
    ;; submods in syncheck:add-arrow/name-dup/pxpy.
    (define ht-defs (make-hash))
    (define (add-def-submods sym beg end mods)
      (hash-set! ht-defs (list sym beg end) mods))
    (define (get-def-submods sym beg end)
      (define subs (hash-ref ht-defs (list sym beg end) #f))
      (and subs (str subs)))

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-definition-target _ beg* end* symbol rev-mods)
      (define beg (add1 beg*))
      (define end (add1 end*))
      (define submods (reverse rev-mods))
      (add-def-submods symbol beg end submods)
      (add-def src beg end submods symbol))

    (define/override (syncheck:add-jump-to-definition _ beg end sym path submods)
      (when (file-exists? path)
        (add-use src (add1 beg) (add1 end) path submods sym)
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
          (add-use src (add1 use-beg) (add1 use-end) src submods sym))))

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
    (queue-more-files-to-analyze (send (current-annotations)
                                       get-more-files-to-analyze))))
