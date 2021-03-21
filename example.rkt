#lang racket/base

(require "db.rkt"
         "analyze.rkt")

(module+ in-memory-example
  (require racket/runtime-path)
  (open 'memory analyze-code)
  (create-tables)

  ;; Re-analyze example/define.rkt and example/require.rkt.
  (define-runtime-path define.rkt "example/define.rkt")
  (define-runtime-path require.rkt "example/require.rkt")
  (forget-digest (build-path define.rkt))
  (forget-digest (build-path require.rkt))
  (analyze-path (build-path require.rkt))
  (define-runtime-path main.rkt "db.rkt")
  (forget-digest (build-path main.rkt))
  (analyze-path (build-path main.rkt)))

(module+ on-disk-example
  (require racket/runtime-path
           rackunit)
  (define-runtime-path db-path "locs.sqlite")
  (create-database db-path)
  (open db-path analyze-code)

  ;; Re-analyze example/define.rkt and example/require.rkt.
  (define-runtime-path define.rkt "example/define.rkt")
  (define-runtime-path require.rkt "example/require.rkt")
  (forget-digest (build-path define.rkt))
  (forget-digest (build-path require.rkt))
  (analyze-path (build-path require.rkt))
  ;; Test that various uses in example/require.rkt point to the
  ;; correct definition location in example/define.rkt.
  (check-equal? (use-pos->def require.rkt 42)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain" 88 93)
                "plain")
  (check-equal? (use-pos->def require.rkt 42)
                (use-pos->def/transitive require.rkt 42)
                "transitive def of non-contract-wrapped is the same")
  (check-equal? (use-pos->def require.rkt 48)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain" 88 93)
                "renamed")
  (check-equal? (use-pos->def require.rkt 56)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted1.1" 207 218)
                "contracted1")
  (check-equal? (use-pos->def/transitive require.rkt 56)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "contracted1" 165 176)
                "contracted1 transitive")
  (check-equal? (use-pos->def require.rkt 68)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted2.1" 283 294)
                "contracted2")
  (check-equal? (use-pos->def/transitive require.rkt 68)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "contracted2" 246 257)
                "contracted2 transitive")
  (check-equal? (use-pos->def require.rkt 80)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted/renamed.1" 363 366)
                "contracted/renamed")
  (check-equal? (use-pos->def/transitive require.rkt 80)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "c/r" 322 325)
                "contracted/renamed transitive")
  (check-equal? (use-pos->def require.rkt 99)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "plain-by-macro" 515 529)
                "plain-by-macro")
  (check-equal? (use-pos->def require.rkt 114)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-contracted-by-macro.1" 684 703)
                "contracted-by-macro")
  (check-equal? (use-pos->def require.rkt 134)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "(sub)" "sub" 958 961)
                "sub")
  (check-equal? (use-pos->def require.rkt 138)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "(sub)" "sub" 958 961)
                "sub/renamed")
  (check-equal? (use-pos->def require.rkt 150)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "foo" 1179 1182)
                "foo")
  (check-equal? (use-pos->def require.rkt 154)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "a-number" 1225 1233)
                "a-number")
  (check-equal? (use-pos->def require.rkt 163)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "a-parameter" 1265 1276)
                "a-parameter")
  (check-equal? (use-pos->def require.rkt 175)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "provide/contract-id-from-m.1" 1421 1427)
                "from-m")
  (check-equal? (use-pos->def require.rkt 182)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "d/c" 1456 1459)
                "d/c")
  (check-equal? (use-pos->def require.rkt 186)
                '#("/home/greg/src/racket/pdb/example/define.rkt"
                   "()" "d/c" 1456 1459)
                "renamed-d/c")

  (check-equal? (get-uses define.rkt '() 'c/r)
                '(#("/home/greg/src/racket/pdb/example/define.rkt"
                    "c/r" 363 366))
                "get-uses")
  (check-equal? (get-uses/transitive define.rkt '() 'c/r)
                '(#("/home/greg/src/racket/pdb/example/define.rkt"
                    "c/r" 363 366)
                  #("/home/greg/src/racket/pdb/example/require.rkt"
                    "provide/contract-id-contracted/renamed.1" 80 98))
                "get-uses/transitive")

  ;; Re-analyze another file (and watch the `definitions` logger topic)
  (define-runtime-path main.rkt "db.rkt")
  (forget-digest (build-path main.rkt))
  (analyze-path (build-path main.rkt)))
