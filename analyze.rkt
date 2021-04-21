#lang racket/base

(require drracket/check-syntax
         racket/contract
         racket/class
         racket/match
         racket/path
         racket/set
         syntax/modread
         (prefix-in db: "db.rkt")
         "contract-hack.rkt"
         "imports-and-exports.rkt")

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
       (analyze-imports-and-exports db:add-import
                                    db:add-export
                                    db:add-rename
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
      (define (mpi->path+submods mpi)
        (match (resolved-module-path-name (module-path-index-resolve mpi))
          [(? path-string? path)                             (values path null)]
          ['|expanded module|                                (values src  null)]
          [(? symbol? sym)                                   (values src  (list sym))]
          [(list (? path-string? path) (? symbol? subs) ...) (values path subs)]
          [(list '|expanded module|    (? symbol? subs) ...) (values src  subs)]
          [(list (? symbol? sym)       (? symbol? subs) ...) (values src (cons sym subs))]))
      (define def-sym (string->symbol (substring code-str def-beg def-end)))
      (define use-sym (string->symbol (substring code-str use-beg use-end)))
      (define-values (from-path from-submods from-sym nom-path nom-submods nom-sym)
        (match (identifier-binding use-stx level)
          [(list* from-mod from-sym nom-mod nom-sym _)
           (define-values (from-path from-submods) (mpi->path+submods from-mod))
           (define-values (nom-path  nom-submods)  (mpi->path+submods nom-mod))
           (values from-path from-submods from-sym nom-path nom-submods nom-sym)]
          [_
           (values src null use-sym src null use-sym)]))
      (define kind (match require-arrow?
                     [#f 'lexical]
                     [#t 'require]
                     [v  v]))
      (db:add-arrow src
                    (add1 use-beg)
                    (add1 use-end)
                    use-sym
                    (syntax->datum use-stx)
                    kind
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

;; borrowed from syncheck/traversals.rkt
(define (get-require-filename datum)
  (define mpi
    (with-handlers ([exn:fail? (λ (x) #f)])
      (cond
        [(module-path-index? datum)
         (module-path-index-resolve datum)]
        [else
         ((current-module-name-resolver) datum #f #f #t)])))
  (define rkt-path/mod-path (and mpi (resolved-module-path-name mpi)))
  (define rkt-path/f (cond
                       [(path? rkt-path/mod-path) rkt-path/mod-path]
                       [(and (pair? rkt-path/mod-path)
                             (path? (car rkt-path/mod-path)))
                        (car rkt-path/mod-path)]
                       [else #f]))
  (define rkt-submods (cond
                        [(not rkt-path/mod-path) #f]
                        [(or (symbol? rkt-path/mod-path) (path? rkt-path/mod-path)) '()]
                        [(pair? rkt-path/mod-path) (cdr rkt-path/mod-path)]))
  (define cleaned-up-path
    (let/ec k
      (unless (path? rkt-path/f) (k rkt-path/f))
      (when (file-exists? rkt-path/f) (k rkt-path/f))
      (let* ([bts (path->bytes rkt-path/f)]
             [len (bytes-length bts)])
        (unless (and (len . >= . 4)
                     (bytes=? #".rkt" (subbytes bts (- len 4))))
          (k rkt-path/f))
        (let ([ss-path (bytes->path (bytes-append (subbytes bts 0 (- len 4)) #".ss"))])
          (unless (file-exists? ss-path)
            (k rkt-path/f))
          ss-path))))
  (values cleaned-up-path rkt-submods))

