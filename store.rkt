;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang at-exp racket/base

(require db
         racket/format
         racket/match
         (only-in racket/path simple-form-path)
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
         all-known-paths
         files-nominally-importing)

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

(define (connect/add-flush)
  (define dbc (sqlite3-connect #:database  (db-file)
                               #:mode      'read/write
                               #:use-place #t))
  (plumber-add-flush! (current-plumber)
                      (λ _ (disconnect dbc)))
  dbc)

(define dbc (connect/add-flush))

(define-simple-macro (with-transaction e:expr ...+)
  (call-with-transaction dbc (λ () e ...)))

(with-transaction
  ;; Simple versioning: Store an expected version string in a table
  ;; named "version". Unless found, re-create all the tables.
  (define expected-version 6) ;use INTEGER here, beware sqlite duck typing
  (define actual-version (with-handlers ([exn:fail? (λ _ #f)])
                           (query-maybe-value dbc (select version #:from version))))
  (unless (equal? actual-version expected-version)
    (log-pdb-warning "Found db version ~v but need ~v; re-creating db tables"
                     actual-version
                     expected-version)
    (for ([table (in-list '("version" "files" "paths" "exports" "imports"))])
      (query-exec dbc (format "drop table if exists ~a" table)))
    (query-exec dbc (create-table version #:columns [version string]))
    (query-exec dbc (insert #:into version #:set [version ,expected-version])))

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

  ;; These three tables allow _efficiently_ looking up, for some
  ;; export, which known files nominally import it. (Without this,
  ;; you'd need to examine all import arrows for all known files,
  ;; which of course is slow when we know about many files.)
  ;;
  ;; This is used solely by def->uses in the implementation of
  ;; rename-sites.
  ;;
  ;; Although this could be expressed as just two tables -- exports
  ;; and imports -- we complicate it a little by using a third table
  ;; to "intern" path name strings. Definitely saves space. Possibly
  ;; speeds some comparisions. Somewhat slows insertions.
  ;;
  ;; Here we're using sqlite more in the spirit of a sql database
  ;; with normalized tables and relational queries.
  (query-exec dbc
              (create-table
               #:if-not-exists paths
               #:columns
               [path_id integer]
               [path    string]
               #:constraints
               (primary-key path_id)
               (unique path)))
  (query-exec dbc
              (create-table
               #:if-not-exists exports
               #:columns
               [export_id integer]
               [path_id   integer]
               [ibk       string]
               #:constraints
               (primary-key export_id)
               (unique path_id ibk)
               (foreign-key path_id #:references (paths path_id))))
  (query-exec dbc
              (create-table
               #:if-not-exists imports
               #:columns
               [export_id integer]
               [path_id   integer]
               #:constraints
               (primary-key path_id export_id)
               (foreign-key path_id #:references (paths path_id))
               (foreign-key export_id #:references (exports export_id)))))

(define (forget path)
  (with-transaction
    (remove-file-from-sqlite path)
    (forget-nominal-imports-by path)))

(define (put path file digest exports-used)
  (with-transaction
    (write-file+digest-to-sqlite path file digest)
    (add-nominal-imports path exports-used)))

;; `files` table

(define (write-file+digest-to-sqlite path data digest)
  (define path-str (path->string path))
  (define compressed-data
    (gzip-bytes
     (write-to-bytes
      (serialize
       (file-before-serialize data)))))
  (with-transaction ;"upsert"
    (query-exec dbc
                (delete #:from files #:where (= path ,path-str)))
    (query-exec dbc
                (insert #:into files #:set
                        [path   ,path-str]
                        [digest ,digest]
                        [data   ,compressed-data]))))

(struct file+digest (file digest))

(define (get-digest path)
  (query-maybe-value dbc
                     (select digest
                             #:from files
                             #:where (= path ,(path->string path)))))

;; This is written so that when `desired-digest` is not false, and it
;; doesn't match the digest column, we can avoid all the work of
;; unzipping, reading, deserializing, and adjusting the data column.
(define (get-file+digest path desired-digest)
  (define path-str (path->string path))
  (match (query-maybe-row dbc
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
  (query-exec dbc
              (delete #:from files #:where (= path ,path-str))))

(define (all-known-paths)
  (map string->path (query-list dbc (select path #:from files))))

;;; Nominal imports

(define (add-nominal-imports path exports-used)
  ;; Assumes called within transaction.
  (define (add export-path+ibk import-path) ;(-> (cons complete-path? struct?) complete-path? any)
    ;; assumes called within transaction of add-from-hash
    (define export-path-id (add-path (car export-path+ibk)))
    (define export-id (add-export export-path-id (cdr export-path+ibk)))
    (define import-path-id (add-path import-path))
    (add-import import-path-id export-id)
    (void))

  (define (add-path path) ;idempotent; return path_id
    (define path-string (path->string path))
    (query-exec dbc
                (insert #:into paths #:set [path ,path-string] #:or-ignore))
    (query-value dbc
                 (select path_id #:from paths #:where (= path ,path-string))))

  (define (add-export export-path-id ibk) ;idempotent; return export_id
    ;; assumes called within transaction of add-from-hash
    (define ibk-string (struct->string ibk))
    (query-exec dbc
                (insert #:into exports #:set
                        [path_id ,export-path-id]
                        [ibk     ,ibk-string]
                        #:or-ignore))
    (query-value dbc
                 (select export_id
                         #:from exports
                         #:where (and (= path_id ,export-path-id)
                                      (= ibk ,ibk-string)))))

  (define (add-import import-path-id export-id)
    (query-exec dbc
                (insert #:into imports #:set
                        [path_id   ,import-path-id]
                        [export_id ,export-id]
                        #:or-ignore)))
  ;; Assumes called within transaction.
  (forget-nominal-imports-by path)
  (for ([path+ibk (in-set exports-used)])
    (add path+ibk path)))

(define (forget-nominal-imports-by path)
  (query-exec
   dbc
   (delete #:from imports
           #:where (= path_id (select path_id
                                      #:from paths
                                      #:where (= path ,(path->string path)))))))

(define (files-nominally-importing export-path+ibk)
  ;; (-> (cons/c complete-path? struct?) (listof complete-path?))
  (define export-path-string (path->string (car export-path+ibk)))
  (define ibk-string (struct->string (cdr export-path+ibk)))
  (map string->path
       (query-list
        dbc
        (select path
                #:from
                (inner-join
                 (select imports.path_id
                         #:from (inner-join exports imports #:using export_id)
                         #:where (and (= exports.path_id
                                         (select path_id
                                                 #:from paths
                                                 #:where (= path ,export-path-string)))
                                      (= exports.ibk ,ibk-string)))
                 paths #:using path_id)))))

(define (struct->string ibk) ;stripping the struct name
  (format "~a" (cdr (vector->list (struct->vector ibk)))))

(module+ ex ;not a test because actually writes to db
  (require (for-syntax racket/base
                       racket/sequence)
           rackunit
           racket/runtime-path)
  (define-syntax-parser define-runtime-paths
    [(_ id:id ...+)
     #:with (str ...) (for/list ([v (in-syntax #'(id ...))])
                        #`#,(symbol->string (syntax->datum v)))
     #'(begin
         (define-runtime-path id (build-path str)) ...)])
  (define-runtime-paths use-path-1 use-path-2 export-path-1 export-path-2)
  (struct ibk (phase mods sym) #:prefab)
  (add-nominal-imports use-path-1
                       (set (cons export-path-1 (ibk 0 '() 'export-a))
                            (cons export-path-2 (ibk 0 '() 'export-b))))
  (add-nominal-imports use-path-2
                       (set (cons export-path-1 (ibk 0 '() 'export-a))
                            (cons export-path-2 (ibk 0 '() 'export-c))))
  (check-equal? (files-nominally-importing (cons export-path-1 (ibk 0 '() 'export-a)))
                (list use-path-1 use-path-2))
  (check-equal? (files-nominally-importing (cons export-path-2 (ibk 0 '() 'export-b)))
                (list use-path-1))
  (check-equal? (files-nominally-importing (cons export-path-2 (ibk 0 '() 'export-c)))
                (list use-path-2))
  (forget-nominal-imports-by use-path-2)
  (check-equal? (files-nominally-importing (cons export-path-1 (ibk 0 '() 'export-a)))
                (list use-path-1))
  (check-equal? (files-nominally-importing (cons export-path-2 (ibk 0 '() 'export-c)))
                (list)))

(module+ maintenance
  (require racket/string
           "data-types.rkt"
           "span-map.rkt")
  (provide vacuum
           db-stats
           file-stats)

  (define (vacuum)
    (query-exec dbc "vacuum;"))

  (define (db-stats)
    (with-transaction
      (define file-count (query-value dbc (select (count-all) #:from files)))
      (define file-data-size
        (query-value dbc (select (+ (sum (length digest)) (sum (length data)))
                                 #:from files)))
      (define path-count (query-value dbc (select (count-all) #:from paths)))
      (define path-size (query-value dbc (select (sum (length path)) #:from paths)))
      (define export-count (query-value dbc (select (count-all) #:from exports)))
      (define export-size (query-value dbc (select (sum (length ibk))  #:from exports)))
      (define import-count (query-value dbc (select (count-all) #:from imports)))
      (define sqlite-file (db-file))
      (define (MB n)
        (~a (~r (/ n 1024.0 1024.0) #:precision 1) " MiB"))
      @~a{Analysis data for @file-count source files: @(MB file-data-size).

          @import-count nominal imports of @export-count exports: @(MB export-size).
          @path-count interned paths: @(MB path-size).

          Total: @(MB (+ file-data-size path-size export-size)).
          Does not include space for integer key columns or indexes.

          @|sqlite-file|: @(MB (file-size sqlite-file)).
          Actual space on disk may be much larger due to deleted items: see VACUUM.}))

  (define (file-stats path)
    (define size
      (query-maybe-value dbc
                         (select (length data)
                                 #:from files
                                 #:where (= path ,(path->string path)))))
    (match-define (file+digest f d) (get-file+digest path #f))
    (define acccessors+counters
      (list (cons file-syncheck-arrows set-count)
            (cons file-syncheck-definition-targets hash-count)
            (cons file-syncheck-tail-arrows set-count)
            (cons file-syncheck-jumps span-map-count)
            (cons file-syncheck-prefixed-requires span-map-count)
            (cons file-syncheck-mouse-overs span-map-count)
            (cons file-syncheck-docs-menus span-map-count)
            (cons file-syncheck-unused-requires span-map-count)
            (cons file-syncheck-require-opens span-map-count)
            (cons file-syncheck-text-types span-map-count)
            (cons file-pdb-errors span-map-count)
            (cons file-pdb-exports hash-count)
            (cons file-pdb-imports set-count)
            (cons file-pdb-import-renames set-count)
            (cons file-pdb-export-renames set-count)
            (cons file-pdb-sub-range-binders hash-count)))
    (define labels+counts
      (cons
       (cons "KiB compressed in db"
              @(~r ( / size 1024.0) #:precision 1))
       (for/list ([v (in-list acccessors+counters)])
         (match-define (cons accessor counter) v)
         (cons (substring (~a (object-name accessor)) 5)
               (~a (counter (accessor f)))))))
    (define width (for/fold ([n 0])
                            ([count (in-list (map cdr labels+counts))])
                    (max n (string-length count))))
    (string-join (cons
                  (~v path)
                  (for/list ([v (in-list labels+counts)])
                    (match-define (cons label count) v)
                    (~a "  " (~a count #:width width #:align 'right) " " label)))
                 "\n"))

  (require racket/path)
  (displayln (file-stats (simple-form-path "example/define.rkt"))))
