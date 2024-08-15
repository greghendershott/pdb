;; Copyright (c) 2021-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require racket/match
         racket/path
         (only-in raco/command-name
                  short-program+command-name)
         "main.rkt")

(define (err format-string . args)
  (apply eprintf
         (string-append (short-program+command-name) ": " format-string "\n")
         args))

(define (parse vec)
  (match (vector->list vec)
    [(list* "analyze"
            (? path-string? (app simple-form-path path)) _)
     (define depth (or (and (= (vector-length vec) 3)
                            (match (vector-ref vec 2)
                              ["0" 0]
                              ["1" 1]
                              ["all" 9999]))
                       0))
     (cond
       [(directory-exists? path)
        (list 'analyze 'dir path depth)]
       [(file-exists? path)
        (list 'analyze 'file path depth)]
       [else
        (err "~v is not an existing file or directory" path)])]
    [(list "forget" (? path-string? (app simple-form-path path)))
     (cond
       [(equal? path (path-only path)) ;directory?
        (list 'forget 'dir path)]
       [else
        (list 'forget 'file path )])]
    [(list "stats")
     (displayln (db-stats))]
    [(list "stats" (app simple-form-path path))
     (displayln (file-stats path))]
    [_
     (err "~v is not a valid command" vec)
     (usage)]))

(define (usage)
  (eprintf "todo\n"))

(module+ example
  (parse (vector "analyze" "cli.rkt"))
  (parse (vector "analyze" "cli.rkt" "1"))
  (parse (vector "analyze" "cli.rkt" "all"))
  (parse (vector "analyze" (current-directory)))
  (parse (vector "analyze" (current-directory) "1"))
  (parse (vector "analyze" (current-directory) "all"))

  (parse (vector "forget" "cli.rkt"))
  (parse (vector "forget" (current-directory)))

  (parse (vector "stats"))
  (parse (vector "stats" "cli.rkt")))

#;(parse (current-command-line-arguments ))



