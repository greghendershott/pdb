;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require db
         racket/contract
         "common.rkt")

(provide maybe-create/connect)

(define/contract (maybe-create/connect db-path create-proc)
  (-> complete-path? (-> connection? any) any)
  (unless (file-exists? db-path)
    (log-pdb-info "~a does not exist; creating" db-path)
    (define dbc (sqlite3-connect #:database  db-path
                                 #:mode      'create
                                 #:use-place #f))
    (call-with-transaction dbc (λ () (create-proc dbc)))
    (disconnect dbc))
  (define dbc (sqlite3-connect #:database  db-path
                               #:mode      'read/write
                               #:use-place #t))
  (plumber-add-flush! (current-plumber) (λ _ (disconnect dbc)))
  dbc)

