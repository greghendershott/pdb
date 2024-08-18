;; Copyright (c) 2021-2024 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require racket/cmdline
         racket/format
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
  (apply eprintf format-string args)
  (newline (current-error-port))
  (exit 1))

(define current-analyze-depth (make-parameter 0))
(define current-analyze-always? (make-parameter #f))

(define (analyze-file-or-dir path)
  (cond
    [(directory-exists? path)
     (void (add-directory path
                          #:import-depth (current-analyze-depth)
                          #:always? (current-analyze-always?)))]
    [(file-exists? path)
     (unless (fresh-analysis?
              (analyze-path path
                            #:import-depth (current-analyze-depth)
                            #:always? (current-analyze-always?)))
       (displayln "Already in cache"))]
    [else
     (err "~v is not an existing file or directory" path)]))

(define (forget-file-or-dir path)
  (cond
    [(equal? path (path-only path)) ;directory?
     (forget-directory path)]
    [else
     (forget-path path)]))

(define (parse vec)
  (define log-level 'info)
  (command-line
   #:program (short-program+command-name)
   #:argv (match vec [(vector) (vector "--help")] [v v])
   #:once-any
   [("-v" "--verbose") "Verbose messages"
                       (set! log-level 'info)]
   [("-V" "--very-verbose") "Very verbose messages"
                            (set! log-level 'debug)]
   [("-s" "--silent") "Silent; minimal messages"
                      (set! log-level 'warning)]
   #:ps
   ""
   "For help on a particular subcommand, use 'raco pdb <subcommand> --help'."
   "  raco pdb analyze     Analyze file(s)"
   "  raco pdb add         Alias for 'analyze'"
   "  raco pdb forget      Forget analysis of file(s)"
   "  raco pdb stats       Show stats for a file or entire db"
   #:args (subcommand . option/arg)
   (with-logging-to-port
    #:logger pdb-logger
    (current-error-port)
    (λ () (parse-subcommand subcommand option/arg))
    log-level 'pdb)))

(define (parse-subcommand subcommand more)
  (match subcommand
    [(or "analyze" "add")
     (command-line
      #:program (~a (short-program+command-name) " add")
      #:argv more
      #:once-each
      [("-a" "--always") "Always analyze, even if already cached."
                         (current-analyze-always? #t)]
      #:once-any
      [("-d" "--depth") depth
                        ("Analyze imported files transitively to this depth."
                         "Reasonable values are 0 (the default) or 1."
                         "See also --max-depth.")
                        (current-analyze-depth depth)]
      [("-D" "--max-depth") ("Maximally analyze imported files transitively."
                             "Analyzes the full import chains up to opaque modules"
                             "such as #%core or #%runtime.")
                            (current-analyze-depth 99999)]
      #:args (file-or-dir)
      (analyze-file-or-dir (simple-form-path file-or-dir)))]
    ["forget"
     (command-line
      #:program (~a (short-program+command-name) " forget")
      #:argv more
      #:args (file-or-dir)
      (forget-file-or-dir (simple-form-path file-or-dir)))]
    ["stats"
     (command-line
      #:program (~a (short-program+command-name) " stats")
      #:argv more
      #:args maybe-file
      (displayln
       (if (null? maybe-file)
           (db-stats)
           (file-stats (simple-form-path (car maybe-file))))))]
    [v (err "Not a valid subcommand: ~v.\nUse 'raco pdb --help'." v)]))

(parse (current-command-line-arguments))

