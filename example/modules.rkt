#lang racket/base
(require net/url)
get-pure-port

(module+ m+
  (require racket/path)
  simple-form-path
  get-pure-port)

(module m racket/base
  (module n racket/base
    (module o racket/base)))

(module+ m+
  (require racket/file)
  file->bytes)

(module+ m+
  (module+ n+
    file->bytes))

#;
(module* m* racket/base
  (require (submod ".."))
  get-pure-port)
