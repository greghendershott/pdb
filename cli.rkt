;; Copyright (c) 2021-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/format
         racket/logging
         racket/match
         racket/path
         (only-in raco/command-name
                  short-program+command-name)
         "analyze.rkt"
         "common.rkt"
         (only-in (submod "store.rkt" stats)
                  db-stats
                  file-stats))

(define (err format-string . args)
  (apply eprintf
         (string-append (short-program+command-name) ": " format-string "\n")
         args)
  (exit 1))

(define (analyze-file-or-dir path [depth 0])
  (cond
    [(directory-exists? path)
     (void (add-directory path #:import-depth depth))]
    [(file-exists? path)
     (unless (fresh-analysis? (analyze-path path #:import-depth depth))
       (displayln "Already in cache"))]
    [else
     (err "~v is not an existing file or directory" path)]))

(define (forget-file-or-dir path)
  (cond
    [(equal? path (path-only path)) ;directory?
     (forget-directory path)]
    [else
     (forget-path path)]))

(define (parse* vec)
  (match vec
    [(vector (or "analyze" "add")
             (app simple-form-path path)
             (app string->number depth))
     (analyze-file-or-dir path (or depth 0))]
    [(vector (or "analyze" "add")
             (app simple-form-path path))
     (analyze-file-or-dir path)]
    [(vector (or "analyze" "add"))
     (analyze-file-or-dir (simple-form-path (current-directory)))]
    [(vector "forget"
             (app simple-form-path path))
     (forget-file-or-dir path)]
    [(vector "forget")
     (forget-file-or-dir (simple-form-path (current-directory)))]
    [(vector "stats")
     (displayln (db-stats))]
    [(vector "stats"
             (app simple-form-path path))
     (displayln (file-stats path))]
    [_
     (usage)
     (exit 1)]))

(define (parse vec)
  (with-logging-to-port
    #:logger pdb-logger
    (current-error-port)
    (λ () (parse* vec))
    'info 'pdb))

(define (usage)
  (displayln
   @~a{Usage:

       raco pdb add [<file-or-directory> [<import-depth>]]

       <file-or-directory> defaults to the current directory.
       <import-depth> defaults to 0, and says how far to analyze
       transitively imported files.
       
       raco pdb forget [<file-or-directory>]

       <file-or-directory> defaults to the current directory.

       raco pdb stats [<file>]

       When <file> is supplied, show stats about the file.
       Otherwise show stats about the entire database.
       }
   (current-error-port)))

(parse (current-command-line-arguments))
