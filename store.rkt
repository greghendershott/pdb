;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require db
         racket/match
         (only-in racket/path simple-form-path)
         racket/serialize
         racket/set
         sql
         syntax/parse/define
         "common.rkt"
         "gzip.rkt"
         (only-in "data-types.rkt"
                  file-massage-before-serialize
                  file-massage-after-deserialize))

(provide (rename-out [read-file-from-sqlite get-file/bypass-cache])
         get-file
         forget
         put
         add-path-if-not-yet-known
         all-known-paths
         files-nominally-importing)

;;; The store consists of a sqlite db, as wel as a write-through cache
;;; for the `files` table.

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
  ;; This is the main table. Each row corresponds to an analyzed file.
  ;; The first column is the path; the other column is the gzipped,
  ;; `write` bytes of a serialized value. (Although the value is a
  ;; `file` struct, this file is written not to know or care that,
  ;; apart from using the file-massage-{before after}-{serialize
  ;; deserialize} functions.)
  ;;
  ;; Here we're really just using sqlite as an alternative to writing
  ;; individual .rktd files all over the user's file system.
  (query-exec dbc
              (create-table
               #:if-not-exists files
               #:columns
               [path string]
               [data blob]
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

;; This acts as a write-through cache for the storage in the sqlite
;; db. We want things like analyze-path and get-mouse-overs etc. to
;; work fast for the small working set of files the user is editing.
;; However things like def->uses/same-name use read-file-from-sqlite
;; to avoid populating the cache, thereby preserving the working set.
(struct entry (time file))
(define cache (make-hash)) ;complete-path? => entry?
(define current-cache-maximum-entries (make-parameter 32))
(define sema (make-semaphore 1))
(define-simple-macro (with-semaphore e:expr ...+)
  (call-with-semaphore sema (λ () e ...)))

(define (get-file path)
  (with-semaphore
    (define (set/prune f)
      (hash-set! cache path (entry (current-seconds) f))
      (maybe-remove-oldest!)
      f)
    (cond [(hash-ref cache path #f)     => entry-file]
          [(read-file-from-sqlite path) => set/prune]
          [else                         #f])))

(define (forget path)
  (with-semaphore
    (hash-remove! cache path)
    (with-transaction
      (remove-file-from-sqlite path)
      (forget-nominal-imports-by path))))

(define (put path file exports-used)
  (with-semaphore
    (hash-set! cache path (entry (current-seconds) file))
    (maybe-remove-oldest!)
    (with-transaction
      (write-file-to-sqlite path file)
      (add-nominal-imports path exports-used))))

(define (maybe-remove-oldest!)
  ;; assumes called in with-semaphore from get-file or put
  (when (>= (hash-count cache) (current-cache-maximum-entries))
    (define-values (oldest-path _)
      (for/fold ([oldest-path #f]
                 [oldest-time +inf.0])
                ([(path entry) (in-hash cache)])
        (if (< (entry-time entry) oldest-time)
            (values path         (entry-time entry))
            (values oldest-path oldest-time))))
    (hash-remove! cache oldest-path)))

;; `files` table

(define (write-file-to-sqlite path data)
  (define path-str (path->string path))
  (define compressed-data
    (gzip-bytes
     (write-to-bytes
      (serialize
       (file-massage-before-serialize data)))))
  (with-transaction ;"upsert"
    (query-exec dbc
                (delete #:from files #:where (= path ,path-str)))
    (query-exec dbc
                (insert #:into files #:set
                        [path ,path-str]
                        [data ,compressed-data]))))

(define (read-file-from-sqlite path)
  (define path-str (path->string path))
  (match (query-maybe-row dbc
                          (select data #:from files
                                  #:where (= path ,path-str)))
    [(vector compressed-data)
     (with-handlers ([exn:fail?
                      (λ (e)
                        (log-pdb-warning "Error deserializing ~v:\n~a"
                                         path
                                         (exn->string e))
                        #f)])
       (file-massage-after-deserialize
        (deserialize
         (read-from-bytes
          (gunzip-bytes compressed-data)))))]
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

;; Add IFF it doesn't already exist. The intended use here is to write
;; a `file` struct with empty sub-values and a digest of "". This
;; simply records that we know about a file that could be analyzed --
;; and would need to be for something like rename-sites to find more
;; sites -- without needing to do so eagerly. In other words this can
;; act as a to-do list.
(define (add-path-if-not-yet-known path data)
  (define path-str (path->string path))
  (with-transaction
    (unless (query-maybe-value dbc
                               (select path #:from files
                                       #:where (= path ,path-str)))
      (write-file-to-sqlite path data))))

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
