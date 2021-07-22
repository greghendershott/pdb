#lang racket/base

(require syntax/parse/define)

(provide phase+space
         phase+space-phase
         phase+space-space
         phase+space+
         phase+space-shift+)

;; A macro to try to dynamic-require something, supplying a fallback
;; implementation. Useful for things added in verisons of Racket newer
;; than we might be running on. (This is a general-purpose macro we
;; could move to a util.rkt or common.rkt if needed elsehwere.)
(define-simple-macro (define-polyfill (id:id arg:expr ...)
                       #:module mod:id
                       body:expr ...+)
  (define id
    (with-handlers ([exn:fail? (λ (_exn)
                                 (λ (arg ...) body ...))])
      (dynamic-require 'mod 'id))))

;; This just DRYs `#:module racket/phase+space` here in this file.
(define-simple-macro (def (id:id arg:expr ...)
                       body:expr ...+)
  (define-polyfill (id arg ...) #:module racket/phase+space
    body ...))

(def (phase+space phase maybe-space)
  (if maybe-space
      (cons phase maybe-space)
      phase))

(def (phase+space-phase p+s)
  (if (pair? p+s) (car p+s) p+s))

(def (phase+space-space p+s)
  (and (pair? p+s) (cdr p+s)))

(def (phase+space+ p+s s)
  (let ([p1 (if (pair? p+s) (car p+s) p+s)]
        [p2 (if (pair? s) (car s) s)]
        [sp1 (and (pair? p+s) (cdr p+s))])
    (let ([p (and p1 p2 (+ p1 p2))]
          [sp (if (pair? s) (cdr s) sp1)])
      (if sp
          (cons p sp)
          p))))

(def (phase+space-shift+ s1 s2)
  (let ([p1 (if (pair? s1) (car s1) s1)]
        [p2 (if (pair? s2) (car s2) s2)])
    (let ([p (and p1 p2 (+ p1 p2))])
      (if (pair? s2)
          (cons p (cdr s2))
          (if (pair? s1)
              (cons p (cdr s1))
              p)))))
