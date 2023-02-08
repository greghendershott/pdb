#lang racket/base

(require racket/format
         racket/match
         syntax/parse/define)

(provide (all-defined-out))

;;; logger/timing

(define-logger pdb)

(define (time-apply/log what proc args)
  (cond [(log-level? (current-logger) 'debug 'pdb)
         (define old-mem (current-memory-use))
         (define-values (vs cpu real gc) (time-apply proc args))
         (define new-mem (current-memory-use))
         (define delta (- new-mem old-mem))
         (define (fmt n) (~v #:align 'right #:min-width 4 n))
         (log-pdb-debug "~a cpu ~a real ~a gc ~aMiB :: ~a"
                        (fmt cpu) (fmt real) (fmt gc)
                        (~a #:align 'right #:min-width 4
                         (~r #:precision 0 #:groups '(3) #:group-sep ","
                             (/ new-mem 1024.0 1024.0)))
                        what)
         (apply values vs)]
        [else (apply proc args)]))

(define-simple-macro (with-time/log what e ...+)
  (time-apply/log what (λ () e ...) '()))

;;; serializing

(define (str v)
  (cond [(path? v)  (path->string v)]
        [else        (~a v)]))

(define (un-str s)
  (read (open-input-string s)))

;;; identifier-binding

;; This struct corresponds to the 7-item list value returned by
;; identifier-binding, but with the modpath items resolved into path
;; and sub-modules items.
(struct resolved-binding
  (from-path
   from-subs
   from-sym
   from-phase
   nom-path
   nom-subs
   nom-sym
   nom-import-phase
   nom-export-phase)
  #:transparent)

(define (identifier-binding/resolved src id-stx phase default-sym)
  (define (mpi->path+submods mpi)
    (match (resolved-module-path-name (module-path-index-resolve mpi))
      [(? path-string? path)                             (values path null)]
      ['|expanded module|                                (values src  null)]
      [(? symbol? sym)                                   (values src  (list sym))]
      [(list (? path-string? path) (? symbol? subs) ...) (values path subs)]
      [(list '|expanded module|    (? symbol? subs) ...) (values src  subs)]
      [(list (? symbol? sym)       (? symbol? subs) ...) (values src (cons sym subs))]))
  (match (identifier-binding id-stx phase)
    [(list from-mod from-sym nom-mod nom-sym from-phase nom-import-phase nom-export-phase)
     (define-values (from-path from-subs) (mpi->path+submods from-mod))
     (define-values (nom-path  nom-subs)  (mpi->path+submods nom-mod))
     (resolved-binding from-path from-subs from-sym from-phase
                       nom-path nom-subs nom-sym nom-import-phase nom-export-phase)]
    [_
     (resolved-binding src null default-sym phase
                       src null default-sym phase phase)]))
