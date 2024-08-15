;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require "analyze.rkt"
         "query.rkt"
         "relations.rkt"
         "syncheck-api.rkt"
         (only-in (submod "store.rkt" stats)
                  db-stats
                  file-stats))

(provide analyze-path
         fresh-analysis?
         fresh-analysis-expanded-syntax
         forget-path
         add-directory
         forget-directory

         get-annotations
         get-submodule-names
         get-completion-candidates
         get-errors
         get-point-info
         get-doc-link
         get-require-path

         send-to-syncheck-annotations-object

         use->def
         nominal-use->def
         rename-sites

         db-stats
         file-stats)
