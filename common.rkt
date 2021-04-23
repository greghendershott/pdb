#lang racket/base

(require racket/format
         racket/match
         syntax/parse/define)

(provide (all-defined-out))

;;; logger/timing

(define-logger pdb)

(define (time-apply/log what proc args)
  (define-values (vs cpu real gc) (time-apply proc args))
  (define (fmt n) (~v #:align 'right #:min-width 4 n))
  (log-pdb-debug "~a cpu | ~a real | ~a gc <= ~a"
                 (fmt cpu) (fmt real) (fmt gc) what)
  (apply values vs))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;; serializing

(define (str v)
  (cond [(path? v)  (path->string v)]
        [(number? v) v]
        [else        (~a v)]))

(define (un-str s)
  (read (open-input-string s)))

;;; identifier-binding

(define (identifier-binding/resolved src id-stx level default-sym)
  (define (mpi->path+submods mpi)
    (match (resolved-module-path-name (module-path-index-resolve mpi))
      [(? path-string? path)                             (values path null)]
      ['|expanded module|                                (values src  null)]
      [(? symbol? sym)                                   (values src  (list sym))]
      [(list (? path-string? path) (? symbol? subs) ...) (values path subs)]
      [(list '|expanded module|    (? symbol? subs) ...) (values src  subs)]
      [(list (? symbol? sym)       (? symbol? subs) ...) (values src (cons sym subs))]))
  (match (identifier-binding id-stx level)
    [(list* from-mod from-sym nom-mod nom-sym _)
     (define-values (from-path from-submods) (mpi->path+submods from-mod))
     (define-values (nom-path  nom-submods)  (mpi->path+submods nom-mod))
     (values from-path from-submods from-sym nom-path nom-submods nom-sym)]
    [_
     (values src null default-sym src null default-sym)]))
