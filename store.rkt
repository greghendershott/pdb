#lang racket/base

(require db
         file/gzip
         file/gunzip
         racket/match
         racket/runtime-path
         racket/serialize
         sql
         "gzip.rkt")

(provide open
         close
         get-file*
         forget-file
         put-file
         write-through-file
         add-path-if-not-yet-known
         all-known-paths
         for-each-known-path
         current-massage-before-serialize
         current-massage-after-deserialize)

;;;; The store consists of a sqlite db and a write-through cache.

;;; db

;; Each row corresponds to an analyzed file. The first column is the
;; path; the other column is the gzipped, `write` bytes of a
;; serialized value. (The value is a `file` struct, but this file is
;; written not to know or care about that.)

(define-runtime-path db-path "db.sqlite")

(define dbc (make-parameter #f))

(define (create)
  (unless (file-exists? db-path)
    (define dbc (sqlite3-connect #:database  db-path
                                 #:mode      'create
                                 #:use-place #f))
    (query-exec dbc
                (create-table
                 #:if-not-exists files
                 #:columns
                 [path string]
                 [data blob]
                 #:constraints
                 (primary-key path)))
    (disconnect dbc)))

(define (open)
  (unless (file-exists? db-path)
    (create))
  (dbc
   (sqlite3-connect #:database  db-path
                    #:mode      'read/write
                    #:use-place #t)))

(define (close)
  (define v (dbc))
  (when (and (connection? v)
             (connected? v))
    (disconnect v)))

;; 1. To massage data before/after (de)serialization, we use functions
;; defined elsewhere, so that we don't need to expose the `file`
;; struct definition here; this code only cares about reading/writing
;; some arbitrary value.
;;
;; 2. These are parameters because they never vary; the requiring
;; module just sets and forgets, and we needn't supply them as
;; arguments.
(define current-massage-before-serialize (make-parameter void))
(define current-massage-after-deserialize (make-parameter void))

(define (write-file-to-sqlite path data)
  (define path-str (path->string path))
  (define compressed-data
    (gzip-bytes
     (write-to-bytes
      (serialize
       ((current-massage-before-serialize) data)))))
  (call-with-transaction ;"upsert"
   (dbc)
   (λ ()
     (query-exec (dbc)
                 (delete #:from files #:where (= path ,path-str)))
     (query-exec (dbc)
                 (insert #:into files #:set
                         [path ,path-str]
                         [data ,compressed-data])))))

(define (read-file-from-sqlite path)
  (define path-str (path->string path))
  (match (query-maybe-row (dbc)
                          (select data #:from files #:where (= path ,path-str)))
    [(vector compressed-data)
     ((current-massage-after-deserialize)
      (deserialize
       (read-from-bytes
        (gunzip-bytes compressed-data))))]
    [#f #f]))

(define (remove-file-from-sqlite path)
  (define path-str (path->string path))
  (query-exec (dbc)
              (delete #:from files #:where (= path ,path-str))))

(define (add-path-if-not-yet-known path data)
  (define path-str (path->string path))
  (unless (query-maybe-value (dbc) (select path #:from files #:where (= path ,path-str)))
    (write-file-to-sqlite path data)))

(define (all-known-paths)
  (map string->path (query-list (dbc) (select path #:from files))))

(define (for-each-known-path prefix proc)
  (define prefix-str (string-append prefix "%"))
  (define ps (query-list (dbc) (select path #:from files
                                       #:where (like path ,prefix-str))))
  (for* ([p (in-list ps)]
         [p (in-value (string->path p))])
    (proc p
          (hash-ref files p (λ () (read-file-from-sqlite p))))))

;;; cache

;; This acts as a write-through cache for the storage in the sqlite
;; db. We want things like analyze-path and get-mouse-overs etc. to
;; work fast for the small working set of files the user is editing.
;; However things like for-each-known-path, used by
;; def->uses/same-name, avoids populating the cache, thereby
;; preserving the working set.
;;
;; TODO: Limit its size e.g. to N MRU files, or to N bytes memory
;; used.

(define files (make-hash)) ;complete-path? => file?


(define (get-file* path)
  (hash-ref! files path
             (λ () (read-file-from-sqlite path))))

(define (forget-file path)
  (hash-remove! files path)
  (remove-file-from-sqlite path))

(define (put-file path file)
  (hash-set! files path file)
  (write-file-to-sqlite path file))

;; For use when something -- specifically analyze-path -- directly
;; updates some of the fields of the struct resident in the cache,
;; then we want to ensure it's written out to the db.

(define (write-through-file path)
  (write-file-to-sqlite path (hash-ref files path)))

(define (write-to-bytes v)
  (define out (open-output-bytes))
  (write v out)
  (get-output-bytes out))

(define (read-from-bytes bstr)
  (define in (open-input-bytes bstr))
  (read in))
