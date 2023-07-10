;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require db
         racket/format
         racket/match
         (only-in racket/path simple-form-path)
         racket/promise
         racket/serialize
         racket/set
         sql
         syntax/parse/define
         "common.rkt"
         "gzip.rkt"
         [only-in "data-types.rkt"
                  file-before-serialize
                  file-after-deserialize])

(provide (struct-out file+digest)
         get-file
         get-digest
         get-file+digest
         forget
         put
         uses-of-export
         put-resolved-module-path-exports
         get-resolved-module-path-exports)

;;; The store consists of a sqlite db.

;; Determine directory in which to store the sqlite db file,
;; creating the directory if necessary.
(define (db-parent-dir)
  (define dir
    (match (getenv "PDB_DIR")
      [(? path-string? ps)
       (simple-form-path ps)]
      [_
       (define parent (if (directory-exists? (find-system-path 'cache-dir))
                          (find-system-path 'cache-dir)
                          (find-system-path 'home-dir)))
       (path->directory-path (build-path parent "pdb"))]))
  (unless (directory-exists? dir)
    (log-pdb-info "~v does not exist; creating" dir)
    (make-directory dir))
  (log-pdb-info "Using ~v" dir)
  dir)

;; Determine complete path to the sqlite db file, creating the file if
;; necessary.
(define (db-file)
  (define path (build-path (db-parent-dir) "pdb-main.sqlite"))
  (unless (file-exists? path)
    (log-pdb-info "~a does not exist; creating" path)
    (disconnect (sqlite3-connect #:database  path
                                 #:mode      'create
                                 #:use-place #f)))
  path)

(define-simple-macro (with-transaction dbc:expr e:expr ...+)
  (call-with-transaction dbc (λ () e ...)))

(define (connect/add-flush)
  (define dbc (sqlite3-connect #:database  (db-file)
                               #:mode      'read/write
                               #:use-place #f))
  (plumber-add-flush! (current-plumber)
                      (λ _ (disconnect dbc)))

  (define tables '(version
                   files
                   paths
                   strings exports re_exports imports
                   resolved_module_path_exports))

  (define vacuum?
    (with-transaction dbc
      ;; Simple versioning: Store an expected version in a table named
      ;; "version". Unless found, re-create all the tables.
      (define expected-version 20) ;use INTEGER here, beware sqlite duck typing
      (define actual-version (with-handlers ([exn:fail? (λ _ #f)])
                               (query-maybe-value dbc (select version #:from version))))
      (define upgrade? (not (equal? actual-version expected-version)))
      (when upgrade?
        (log-pdb-warning "Found db version ~v but need ~v; re-creating db tables"
                         actual-version
                         expected-version)
        (for ([table (in-list tables)])
          (query-exec dbc (format "drop table if exists ~a" table)))
        (query-exec dbc (create-table version #:columns [version string]))
        (query-exec dbc (insert #:into version #:set [version ,expected-version])))
      upgrade?))

  (when vacuum? ;vacuum doesn't work inside a transaction
    (query-exec dbc "vacuum;"))

  (with-transaction dbc
    ;; This is the main table. Each row corresponds to an analyzed file.
    ;; The first column is the path; the other column is the gzipped,
    ;; `write` bytes of a serialized value. (Although the value is a
    ;; `file` struct, this file is written not to know or care that,
    ;; apart from using the file-{before after}-{serialize deserialize}
    ;; functions.)
    ;;
    ;; Here we're really just using sqlite as an alternative to writing
    ;; individual .rktd files all over the user's file system.
    (query-exec dbc
                (create-table
                 #:if-not-exists files
                 #:columns
                 [path   string]
                 [digest string]
                 [data   blob]
                 #:constraints
                 (primary-key path)))

    ;; This is effectively a cache of module->exports results,
    ;; used to obtain symbols for completion candidates.
    (query-exec dbc
                (create-table
                 #:if-not-exists resolved_module_path_exports
                 #:columns
                 [rmp string] ;resolved module path
                 [data blob]  ;gzip of (seteq symbol?)
                 #:constraints
                 (primary-key rmp)))

    ;; Here we're using sqlite more in the spirit of a sql database
    ;; with normalized tables and relational queries.
    (query-exec dbc
                (create-table
                 #:if-not-exists strings
                 #:columns
                 [str string #:not-null]
                 #:constraints
                 (unique str)))
    (query-exec dbc
                (create-table
                 #:if-not-exists exports
                 #:columns
                 ;; An export with this path and ibk
                 [path_id     integer #:not-null]
                 [ibk_id      integer #:not-null]
                 ;; And uses of this sub-span
                 [ofs         integer #:not-null]
                 [span        integer #:not-null]
                 [sub_sym     string #:not-null]  ;(i.e. this sub-symbol)
                 ;; Is maybe defined at this pos within path.
                 [sub_pos     integer]
                 #:constraints
                 (unique path_id ibk_id ofs span sub_sym sub_pos)
                 (foreign-key path_id #:references (strings id))
                 (foreign-key ibk_id #:references (strings id))))
    (query-exec dbc
                (create-table
                 #:if-not-exists re_exports
                 #:columns
                 ;; An export with this path and ibk
                 [path_id     integer #:not-null]
                 [ibk_id      integer #:not-null]
                 [ofs         integer #:not-null]
                 [span        integer #:not-null]
                 ;; Is re-exported as this other one
                 [use_path_id integer #:not-null]
                 [use_ibk_id  integer #:not-null]
                 #:constraints
                 (unique path_id ibk_id ofs span use_path_id use_ibk_id)
                 (foreign-key path_id #:references (strings id))
                 (foreign-key ibk_id #:references (strings id))
                 (foreign-key use_path_id #:references (strings id))
                 (foreign-key use_ibk_id #:references (strings id))))
    (query-exec dbc
                (create-table
                 #:if-not-exists imports
                 #:columns
                 ;; This source location
                 [use_path_id integer #:not-null]
                 [use_beg     integer #:not-null]
                 [use_end     integer #:not-null]
                 ;; Imports this export
                 [path_id     integer #:not-null]
                 [ibk_id      string #:not-null]
                 #:constraints
                 (unique use_path_id use_beg use_end path_id ibk_id)
                 (foreign-key use_path_id #:references (strings id))
                 (foreign-key path_id #:references (strings id))
                 (foreign-key ibk_id #:references (strings id)))))
  dbc)

(define dbc-promise (delay/thread (connect/add-flush)))
(define (dbc) (force dbc-promise))

(define (forget path)
  (with-transaction (dbc)
    (remove-file-from-sqlite path)
    (forget-exports-imports path)))

(define (put path file digest #:exports exports #:re-exports re-exports #:imports imports)
  (with-transaction (dbc)
    (write-file+digest-to-sqlite path file digest)
    (with-time/log "add-exports-imports"
      (add-exports-imports path exports re-exports imports))))

;;; `files` table

(define (write-file+digest-to-sqlite path data digest)
  (define path-str (path->string path))
  (define compressed-data
    (gzip-bytes
     (write-to-bytes
      (serialize
       (file-before-serialize data)))))
  (with-transaction (dbc) ;"upsert"
    (query-exec (dbc)
                (delete #:from files #:where (= path ,path-str)))
    (query-exec (dbc)
                (insert #:into files #:set
                        [path   ,path-str]
                        [digest ,digest]
                        [data   ,compressed-data]))))

(struct file+digest (file digest))

(define (get-digest path)
  (query-maybe-value (dbc)
                     (select digest
                             #:from files
                             #:where (= path ,(path->string path)))))

;; This is written so that when `desired-digest` is not false, and it
;; doesn't match the digest column, we can avoid all the work of
;; unzipping, reading, deserializing, and adjusting the data column.
(define (get-file+digest path desired-digest)
  (define path-str (path->string path))
  (match (query-maybe-row (dbc)
                          (if desired-digest
                              (select data digest
                                      #:from files
                                      #:where (and (= path ,path-str)
                                                   (= digest ,desired-digest)))
                              (select data digest
                                      #:from files
                                      #:where (= path ,path-str))))
    [(vector compressed-data digest)
     (with-handlers ([exn:fail?
                      (λ (e)
                        (log-pdb-warning "Error deserializing ~v:\n~a"
                                         path
                                         (exn->string e))
                        #f)])
       (file+digest (file-after-deserialize
                     (deserialize
                      (read-from-bytes
                       (gunzip-bytes compressed-data))))
                    digest))]
    [#f #f]))

(define (get-file path)
  (match (get-file+digest path #f)
    [(file+digest file _digest) file]
    [#f #f]))

(define (write-to-bytes v)
  (define out (open-output-bytes))
  (write v out)
  (get-output-bytes out))

(define (read-from-bytes bstr)
  (define in (open-input-bytes bstr))
  (read in))

(define (remove-file-from-sqlite path)
  (define path-str (path->string path))
  (query-exec (dbc)
              (delete #:from files #:where (= path ,path-str))))

;;; Resolved module path exports

(define (put-resolved-module-path-exports resolved-module-path set-of-symbols)
  (define rmp (~v resolved-module-path))
  (define compressed-data (gzip-bytes (write-to-bytes (set->list set-of-symbols))))
  (with-transaction (dbc) ;"upsert"
    (query-exec (dbc)
                (delete #:from resolved_module_path_exports
                        #:where (= rmp ,rmp)))
    (query-exec (dbc)
                (insert #:into resolved_module_path_exports #:set
                        [rmp ,rmp]
                        [data ,compressed-data]))))

(define (get-resolved-module-path-exports resolved-module-path)
  (define rmp (~v resolved-module-path))
  (match (query-maybe-value (dbc)
                            (select data
                                    #:from resolved_module_path_exports
                                    #:where (= rmp ,rmp)))
    [(? bytes? compressed-data)
     (apply seteq (read-from-bytes (gunzip-bytes compressed-data)))]
    [#f (seteq)]))

;;; Exports/imports


;;; Misc

(define (intern v)
  (define str
    (cond [(path? v)   (path->string v)]
          [(struct? v) (~a (cdr (vector->list (struct->vector v))))]
          [else        (~a v)]))
  (query-exec (dbc) (insert #:into strings #:set [str ,str] #:or-ignore))
  (query-value (dbc) (select rowid #:from strings #:where (= str ,str))))

;; Assumes called within transaction.
(define (add-exports-imports path exports re-exports imports)
  (with-transaction (dbc)
    (forget-exports-imports path))

  (define path-id (intern path))

  (for ([export (in-list exports)])
    (match-define (list ibk offset span sub-sym sub-pos) export)
    (query-exec (dbc)
                (insert #:into exports #:set
                        [path_id ,path-id]
                        [ibk_id  ,(intern ibk)]
                        [ofs     ,offset]
                        [span    ,span]
                        [sub_sym ,(~a sub-sym)]
                        [sub_pos ,(false->sql-null sub-pos)]
                        #:or-ignore)))
  (for ([v (in-set re-exports)])
    (match-define (list src-path src-ibk ofs span use-path use-ibk) v)
    (query-exec (dbc)
                (insert #:into re_exports #:set
                        [path_id     ,(intern src-path)]
                        [ibk_id      ,(intern src-ibk)]
                        [ofs         ,ofs]
                        [span        ,span]
                        [use_path_id ,(intern use-path)]
                        [use_ibk_id  ,(intern use-ibk)]
                        #:or-ignore)))

  (for ([import (in-list imports)])
    (match-define (list import-path import-ibk beg end) import)
    (query-exec (dbc)
                (insert #:into imports #:set
                        [path_id     ,(intern import-path)]
                        [ibk_id      ,(intern import-ibk)]
                        [use_path_id ,path-id]
                        [use_beg     ,beg]
                        [use_end     ,end]
                        #:or-ignore))))

;; Assumes called within transaction
(define (forget-exports-imports path)
  (define path-id (intern path))
  (query-exec (dbc)
              (delete #:from exports
                      #:where (= path_id ,path-id)))
  (query-exec (dbc)
              (delete #:from re_exports
                      #:where (= use_path_id ,path-id)))
  (query-exec (dbc)
              (delete #:from imports
                      #:where (= use_path_id ,path-id))))

(define (uses-of-export path pos add-use!)
  #;(println (list 'uses-of-export path pos))
  (define path-id (intern path))
  (for ([(path-str beg end)
         (in-query
          (dbc)
          (sql
           (with #:recursive
                 ([(rec path_id ibk_id ofs span)
                   (union
                    (select path_id ibk_id ofs span
                            #:from (inner-join exports strings
                                               #:on (= exports.path_id strings.rowid))
                            #:where (and (= path_id ,path-id)
                                         (<= sub_pos ,pos)
                                         (< ,pos (+ sub_pos span))))
                    (select re.use_path_id re.use_ibk_id (+ rec.ofs re.ofs) rec.span
                            #:from (inner-join
                                    rec (as re_exports re)
                                    #:using path_id ibk_id)))])
                 (select #:distinct
                         (as (select str #:from strings
                                     #:where (= imports.use_path_id strings.rowid))
                             use_path)
                         (as (+ imports.use_beg ofs)      use_beg)
                         (as (+ imports.use_beg ofs span) use_end)
                         #:from (inner-join
                                 rec imports
                                 #:on (and (= rec.path_id imports.path_id)
                                           (= rec.ibk_id  imports.ibk_id)))))))])
    (add-use! (string->path path-str) beg end)))

;; Stats

(module+ stats
  (require racket/string
           "data-types.rkt"
           "span-map.rkt")
  (provide db-stats
           file-stats)

  (define (db-stats)
    (with-transaction (dbc)
      (define file-count (query-value (dbc) (select (count-all) #:from files)))
      (define file-data-size
        (query-value (dbc) (select (+ (sum (length digest)) (sum (length data)))
                                   #:from files)))
      (define exports-count (query-value (dbc) (select (count-all) #:from exports)))
      (define exports-size (query-value (dbc) (select (sum (+ 20 (length sub_sym))) #:from exports)))
      (define re-exports-count (query-value (dbc) (select (count-all) #:from re_exports)))
      (define re-exports-size (* 6 4 re-exports-count)) ;6 32-bit ints (?)
      (define imports-count (query-value (dbc) (select (count-all) #:from imports)))
      (define imports-size (* 5 4 imports-count )) ;5 32-bit ints (?)
      (define rmp-export-syms-count (query-value (dbc) (select (count-all) #:from resolved_module_path_exports)))
      (define rmp-export-syms-size (query-value (dbc) (select (sum (length data)) #:from resolved_module_path_exports)))
      (define strings-count (query-value (dbc) (select (count-all) #:from strings)))
      (define strings-size (query-value (dbc) (select (sum (length str)) #:from strings)))
      (define sqlite-file (db-file))
      (define (N n)
        (~a (~r n #:precision 0 #:group-sep "," #:min-width 10)))
      (define (MB n)
        (let ([n (if (sql-null? n) 0 n)])
          (~a (~r (/ n 1024.0 1024.0) #:precision 2 #:min-width 4) " MiB")))
      @~a{--------------------------------------------------------------------------
          Estimated sizes

          @(N file-count) source files analysis data: @(MB file-data-size).

          @(N rmp-export-syms-count) resolved module path export symbol sets: @(MB rmp-export-syms-size).

          @(N exports-count) exports:    @(MB exports-size).
          @(N re-exports-count) re-exports: @(MB re-exports-size).
          @(N imports-count) imports:    @(MB imports-size).
          @(N strings-count) strings:    @(MB strings-size).

          @|sqlite-file| file size: @(MB (file-size sqlite-file)).
          Might include space from deleted items that could be vacuumed.
          -------------------------------------------------------------------------}))

  (define (file-stats path)
    (match (get-file+digest path #f)
      [(file+digest f _d)
       (define size (or (query-maybe-value
                         (dbc)
                         (select (length data)
                                 #:from files
                                 #:where (= path ,(path->string path))))
                        0))
       (define (count v)
         (cond [(set? v)          (set-count v)]
               [(set-mutable? v)  (set-count v)]
               [(hash? v)         (hash-count v)]
               [(span-map? v)     (span-map-count v)]
               [(interval-map? v) (length (dict-values v))]
               [else              "???"]))
       (define labels+counts
         (cons
          (cons "KiB compressed in db"
                @(~r ( / size 1024.0) #:precision 1))
          (for/list ([accessor (in-list (list file-syncheck-arrows
                                              file-syncheck-definition-targets
                                              file-syncheck-tail-arrows
                                              file-syncheck-jumps
                                              file-syncheck-prefixed-requires
                                              file-syncheck-mouse-overs
                                              file-syncheck-docs-menus
                                              file-syncheck-unused-requires
                                              file-syncheck-require-opens
                                              file-syncheck-text-types
                                              file-pdb-errors
                                              file-pdb-modules
                                              file-pdb-definitions
                                              file-pdb-exports
                                              file-pdb-imports
                                              file-pdb-import-renames
                                              file-pdb-export-renames))])
            (cons (substring (~a (object-name accessor)) 5)
                  (~a (count (accessor f)))))))
       (define width (for/fold ([n 0])
                               ([count (in-list (map cdr labels+counts))])
                       (max n (string-length count))))
       (string-join (cons
                     (~v path)
                     (for/list ([v (in-list labels+counts)])
                       (match-define (cons label count) v)
                       (~a "  " (~a count #:width width #:align 'right) " " label)))
                    "\n")]
      [_ (~a path "\nNo analysis in db.")]))

  (module+ ex-1
    (require racket/path)
    (displayln (file-stats (simple-form-path "example/define.rkt"))))

  (module+ ex-2
    (require syntax/modresolve)
    (displayln (file-stats (resolve-module-path 'racket/private/class-internal)))))

(module+ debug
  (define (create-temp-views)
    (query-exec (dbc) "drop view if exists exports_view")
    (query-exec
     (dbc)
     (create-view
      #:temporary
      exports_view
      (select
       (as (select str #:from strings #:where (= path_id rowid)) path)
       (as (select str #:from strings #:where (= ibk_id rowid))  ibk)
       ofs
       span
       sub_sym
       sub_pos
       #:from exports)))
    (query-exec (dbc) "drop view if exists re_exports_view")
    (query-exec
     (dbc)
     (create-view
      #:temporary
      re_exports_view
      (select
       (as (select str #:from strings #:where (= path_id rowid)) path)
       (as (select str #:from strings #:where (= ibk_id rowid))  ibk)
       ofs
       span
       (as (select str #:from strings #:where (= use_path_id rowid)) use_path)
       (as (select str #:from strings #:where (= use_ibk_id rowid))  use_ibk)
       #:from re_exports)))
    (query-exec (dbc) "drop view if exists imports_view")
    (query-exec
     (dbc)
     (create-view
      #:temporary
      imports_view
      (select
       (as (select str #:from strings #:where (= use_path_id rowid)) use_path)
       use_beg
       use_end
       (as (select str #:from strings #:where (= path_id rowid)) path)
       (as (select str #:from strings #:where (= ibk_id rowid))  ibk)
       #:from imports))))

  (define prefix-define.rkt "/home/greg/src/racket/pdb/example/prefix-define.rkt")
  (define prefix-require.rkt "/home/greg/src/racket/pdb/example/prefix-require.rkt")
  (create-temp-views)
  #;
  (query (dbc) (select * #:from exports_view
                       #:where (= path ,prefix-define.rkt)))
  #;
  (query (dbc) (select * #:from re_exports_view
                       #:where (= path ,prefix-define.rkt)))
  #;
  (query (dbc) (select * #:from imports_view
                       #:where (= use_path ,prefix-require.rkt))))
