#lang racket/base

(require racket/runtime-path
         rackunit
         "db.rkt"
         "analyze.rkt")

(define-runtime-path define.rkt "example/define.rkt")
(define define.rkt/str (path->string define.rkt))
(define-runtime-path require.rkt "example/require.rkt")
(define require.rkt/str (path->string require.rkt))

(define (tests)
  ;; Re-analyze example/define.rkt and example/require.rkt.
  (analyze-path (build-path require.rkt) #:always? #t)
  (analyze-path (build-path define.rkt)  #:always? #t)
  ;; Test that various uses in example/require.rkt point to the
  ;; correct definition location in example/define.rkt.
  (check-equal? (use-pos->def require.rkt 42)
                (vector define.rkt/str 88 93)
                "plain")
  (check-equal? (use-pos->def require.rkt 42)
                (use-pos->def/transitive require.rkt 42)
                "transitive def of non-contract-wrapped is the same")
  (check-equal? (use-pos->def require.rkt 48)
                (vector define.rkt/str 88 93)
                "renamed")
  (check-equal? (use-pos->def require.rkt 56)
                (vector define.rkt/str 207 218)
                "contracted1")
  (check-equal? (use-pos->def/transitive require.rkt 56)
                (vector define.rkt/str 165 176)
                "contracted1 transitive")
  (check-equal? (use-pos->def require.rkt 68)
                (vector define.rkt/str 283 294)
                "contracted2")
  (check-equal? (use-pos->def/transitive require.rkt 68)
                (vector define.rkt/str 246 257)
                "contracted2 transitive")
  (check-equal? (use-pos->def require.rkt 80)
                (vector define.rkt/str 363 366)
                "contracted/renamed => c/r")
  (check-equal? (use-pos->def/transitive require.rkt 80)
                (vector define.rkt/str 322 325)
                "contracted/renamed => c/r [transitive]")
  (check-equal? (use-pos->def require.rkt 99)
                (vector define.rkt/str 515 529)
                "plain-by-macro")
  (check-equal? (use-pos->def require.rkt 114)
                (vector define.rkt/str 684 703)
                "contracted-by-macro")
  (check-equal? (use-pos->def require.rkt 134)
                (vector define.rkt/str 958 961)
                "sub")
  (check-equal? (use-pos->def require.rkt 138)
                (vector define.rkt/str 958 961)
                "sub/renamed")
  (check-equal? (use-pos->def require.rkt 150)
                (vector define.rkt/str 1179 1182)
                "foo")
  (check-equal? (use-pos->def require.rkt 154)
                (vector define.rkt/str 1225 1233)
                "a-number")
  (check-equal? (use-pos->def require.rkt 163)
                (vector define.rkt/str 1265 1276)
                "a-parameter")
  (check-equal? (use-pos->def require.rkt 175)
                (vector define.rkt/str 1421 1427)
                "from-m")
  (check-equal? (use-pos->def require.rkt 182)
                (vector define.rkt/str 1456 1459)
                "d/c")
  (check-equal? (use-pos->def require.rkt 186)
                (vector define.rkt/str 1456 1459)
                "renamed-d/c")

  (check-equal? (def-pos->uses define.rkt 88)
                (list
                 (vector define.rkt/str "lexical" "plain" "plain" "plain" "plain" 109 114)
                 (vector define.rkt/str "lexical" "plain" "plain" "plain" "plain" 138 143)
                 (vector define.rkt/str "lexical" "plain" "renamed" "renamed" "renamed" 144 151)
                 (vector require.rkt/str "require" "plain" "plain" "plain" "plain" 42 47)
                 (vector require.rkt/str "require" "plain" "renamed" "renamed" "renamed" 48 55)
                 (vector require.rkt/str "require" "plain" "plain" "PRE:" "PRE:plain" 264 268)
                 (vector require.rkt/str "require" "plain" "plain" "plain" "PRE:plain" 268 273)
                 (vector require.rkt/str "require" "plain" "renamed" "PRE:" "PRE:renamed" 276 280)
                 (vector require.rkt/str "require" "plain" "renamed" "renamed" "PRE:renamed" 280 287)
                 (vector require.rkt/str "require" "plain" "renamed" "plain" "plain" 461 466)
                 (vector require.rkt/str "require" "plain" "plain" "XXX" "XXX" 524 527))
                "def-pos->uses: plain")

  (check-equal? (def-pos->uses define.rkt 322)
                (list
                 (vector define.rkt/str "lexical" "c/r" "c/r" "c/r" "c/r" 363 366))
                "def-pos->uses: c/r"))

(module+ test
  (require sql) ;for ad hoc queries in REPL
  (open 'memory analyze-code)
  (create-tables)
  (tests))

(module+ on-disk-example
  (require sql) ;for ad hoc queries in REPL
  (define-runtime-path db-path "locs.sqlite")
  (create-database db-path)
  (open db-path analyze-code)
  ;; Doing this means analyze-path will queue up more files to
  ;; analyze, transitively, until reaching a fixed point.
  #;(start-analyze-more-files-thread)
  (tests)

  ;; Re-analyze another file (and watch the `definitions` logger topic)
  (define-runtime-path db.rkt "db.rkt")
  (analyze-path (build-path db.rkt) #:always? #t))

(module+ exercise
  (open 'memory analyze-code)
  (create-tables)
  ;; Re-analyze example/define.rkt and example/require.rkt.
  (analyze-path (build-path require.rkt) #:always? #t)
  (analyze-path (build-path define.rkt)  #:always? #t))
