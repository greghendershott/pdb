;; Copyright (c) 2021-2023 by Greg Hendershott.
;; SPDX-License-Identifier: GPL-3.0-or-later

#lang racket/base

(require db
         racket/contract
         sql
         "db.rkt")

(provide put
         forget
         lookup)

;; (hashof path+ibk? (setof path?)) as a sqlite db.
;;
;; Record the files nominally importing an exported path+ibk.

(define (create-tables dbc)
  ;; Although this could be expressed as just two tables -- exports
  ;; and imports -- we complicate it a little by using a third table
  ;; to "intern" path name strings. Definitely saves space. Possibly
  ;; speeds some comparisions. Somewhat slows insertions.
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

(define dbc (maybe-create/connect "pdb-nominal-imports.sqlite" create-tables))

(define/contract (put path ht)
  (-> complete-path? (hash/c (cons/c complete-path? struct?) complete-path?) any)
  (call-with-transaction
   dbc
   (λ ()
     (forget path)
     (add-from-hash-table ht))))

(define (add-from-hash-table ht)
  (call-with-transaction
   dbc
   (λ ()
     (for ([(path+ibk path) (in-hash ht)])
       (add path+ibk path)))))

(define (add export-path+ibk import-path) ;(-> (cons complete-path? struct?) complete-path? any)
  ;; assumes called within transaction of add-from-hash
  (define export-path-id (add-path (car export-path+ibk)))
  (define export-id (add-export export-path-id (cdr export-path+ibk)))
  (define import-path-id (add-path import-path))
  (add-import import-path-id export-id)
  (void))

(define (add-path path) ;idempotent; return path_id
  ;; assumes called within transaction of add-from-hash
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

(define/contract (forget path)
  (-> complete-path? any)
  (query-exec dbc
              (delete #:from imports
                      #:where (= path_id (select path_id
                                                 #:from paths
                                                 #:where (= path ,(path->string path)))))))

(define/contract (lookup export-path+ibk)
  (-> (cons/c complete-path? struct?) (listof complete-path?))
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

#;
(module+ ex
  (open)
  (struct ibk (phase mods sym) #:prefab)
  (add (cons (build-path "export-path") (ibk 0 '() 'export-1))
       (build-path "use-path-1"))
  (add (cons (build-path "export-path") (ibk 0 '() 'export-1))
       (build-path "use-path-2"))
  (add (cons (build-path "export-path-2") (ibk 0 '() 'export-1))
       (build-path "use-path-3"))
  (lookup (cons (build-path "export-path") (ibk 0 '() 'export-1)))
  (forget (build-path "use-path-2"))
  (lookup (cons (build-path "export-path") (ibk 0 '() 'export-1))))
