#lang racket/base

(require drracket/check-syntax
         racket/contract
         racket/class
         racket/match
         racket/path
         racket/set
         syntax/modread
         "analyze-more.rkt"
         (prefix-in db: "db.rkt")
         "common.rkt"
         "contract-hack.rkt")

(provide analyze-code)

(define/contract (analyze-code path code-str)
  (-> path-string? string? any)
  (string->syntax
   path
   code-str
   (λ (stx)
     (parameterize ([current-namespace (make-base-namespace)])
       (define exp-stx (expand stx))
       (analyze-using-check-syntax path exp-stx code-str)
       (maybe-use-hack-for-contract-wrappers db:add-def path exp-stx)
       (analyze-more db:add-import
                     db:add-export
                     db:add-import-rename
                     db:add-export-rename
                     db:add-sub-range-binders
                     path
                     exp-stx)))))

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

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           stx))

    (define/override (syncheck:add-definition-target _useless beg end sym rev-mods)
      (db:add-def src (add1 beg) (add1 end) (reverse rev-mods) sym))

    ;; Note that check-syntax will give us two arrows for prefix-in
    ;; vars.
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      def-stx def-beg def-end _def-px _def-py
                      use-stx use-beg use-end _use-px _use-py
                      _actual? level require-arrow? _name-dup?)
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define-values (from-path from-submods from-sym nom-path nom-submods nom-sym)
        (identifier-binding/resolved src use-stx level use-sym))
      (db:add-arrow src
                    (add1 use-beg)
                    (add1 use-end)
                    use-sym
                    (syntax->datum use-stx)
                    require-arrow?
                    (add1 def-beg)
                    (add1 def-end)
                    def-sym
                    (syntax->datum def-stx)
                    from-path
                    from-submods
                    from-sym
                    nom-path
                    nom-submods
                    nom-sym))

    (define/override (syncheck:add-require-open-menu _ _beg _end file)
      (add-file-to-analyze file))

    (define/override (syncheck:add-mouse-over-status _ beg end str)
      (db:add-mouse-over-status src (add1 beg) (add1 end) str))

    (define/override (syncheck:add-tail-arrow from-stx from-pos to-stx to-pos)
      (when (and (equal? (syntax-source from-stx) src)
                 (equal? (syntax-source to-stx)   src))
        (db:add-tail-arrow src (add1 from-pos) (add1 to-pos))))

    (define/override (syncheck:add-unused-require _ beg end)
      (db:add-unused-require src (add1 beg) (add1 end)))

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

