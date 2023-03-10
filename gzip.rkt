#lang racket/base

(require file/gzip
         file/gunzip
         racket/function)

(provide gzip-bytes
         gunzip-bytes)

(define ((codec-bytes codec) bstr)
  (define in (open-input-bytes bstr))
  (define out (open-output-bytes))
  (codec in out)
  (get-output-bytes out))

(define gzip-bytes   (codec-bytes (curryr gzip-through-ports #f 0)))
(define gunzip-bytes (codec-bytes gunzip-through-ports))

(module+ test
  (require rackunit)
  (define bstr #"asdf;lkjadsfplkjasdfadsfadsf")
  (check-equal? (gunzip-bytes (gzip-bytes bstr))
                bstr))
