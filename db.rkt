;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require db
         racket/contract
         racket/match
         racket/path
         "common.rkt")

(provide maybe-create/connect)

(define db-dir
  (match (getenv "PDB_DIR")
    [(? path-string? ps)
     (simple-form-path ps)]
    [_
     (define parent (if (directory-exists? (find-system-path 'cache-dir))
                        (find-system-path 'cache-dir)
                        (find-system-path 'home-dir)))
     (path->directory-path (build-path parent "pdb"))]))

(unless (directory-exists? db-dir)
  (log-pdb-info "~v does not exist; creating" db-dir)
  (make-directory db-dir))

(log-pdb-info "Using ~v" db-dir)

(define/contract (maybe-create/connect db-file)
  (-> relative-path? any)
  (define db-path (build-path db-dir db-file))
  (unless (file-exists? db-path)
    (log-pdb-info "~a does not exist; creating" db-path)
    (define dbc (sqlite3-connect #:database  db-path
                                 #:mode      'create
                                 #:use-place #f))
    (disconnect dbc))
  (define dbc (sqlite3-connect #:database  db-path
                               #:mode      'read/write
                               #:use-place #t))
  (plumber-add-flush! (current-plumber) (λ _ (disconnect dbc)))
  dbc)

