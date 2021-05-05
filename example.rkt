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

  ;;; uses <=> definitions

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
  (check-equal? (use-pos->def require.rkt 405)
                (vector require.rkt/str 397 404)
                "arrow for only-in rename; needs PR in Racket 8.1.0.3")
  (check-equal? (use-pos->def require.rkt 452)
                (vector require.rkt/str 433 451)
                "arrow for only-in rename; needs PR to Racket 8.1.0.3")

  (check-equal? (def-pos->uses/transitive define.rkt 88)
                (list
                 (vector require.rkt/str "plain" "plain" "plain" 42 47)
                 (vector require.rkt/str "plain" "renamed" "renamed" 48 55)
                 (vector require.rkt/str "plain" "PRE:" "PRE:plain" 264 268)
                 (vector require.rkt/str "plain" "plain" "PRE:plain" 268 273)
                 (vector require.rkt/str "plain" "PRE:" "PRE:renamed" 276 280)
                 (vector require.rkt/str "plain" "renamed" "PRE:renamed" 280 287)
                 (vector require.rkt/str "plain" "renamed" "renamed" 397 404)
                 (vector require.rkt/str "renamed" "plain" "plain" 405 410)
                 (vector require.rkt/str "plain" "plain" "plain" 461 466)
                 (vector require.rkt/str "plain" "plain" "plain" 509 514)
                 (vector require.rkt/str "plain" "XXX" "XXX" 515 518)
                 (vector require.rkt/str "plain" "XXX" "XXX" 524 527)
                 (vector define.rkt/str "plain" "plain" "plain" 109 114)
                 (vector define.rkt/str "plain" "plain" "plain" 138 143)
                 (vector define.rkt/str "plain" "renamed" "renamed" 144 151))
                "def-pos->uses: plain")

  (check-equal? (def-pos->uses define.rkt 322)
                (list
                 (vector define.rkt/str "c/r" "c/r" "c/r" 363 366))
                "def-pos->uses: c/r")

  ;;; uses <=> name-introductions

  (check-equal? (use-pos->name require.rkt 42)
                (vector define.rkt/str 109 114)
                "use-pos->name plain")
  (check-equal? (use-pos->name/transitive require.rkt 42)
                (vector define.rkt/str 88 93)
                "use-pos->name/transitive: plain")
  (check-equal? (use-pos->name require.rkt 48)
                (vector define.rkt/str 144 151)
                "use-pos->name: renamed")
  (check-equal? (use-pos->name require.rkt 264)
                (vector require.rkt/str 242 246)
                "use->pos->name: prefix-in `PRE:` part of `PRE:plain`")
  (check-equal? (use-pos->name require.rkt 268)
                (vector define.rkt/str 109 114)
                "use->pos->name: prefix-in `plain` part of `PRE:plain`")
  (check-equal? (use-pos->name/transitive require.rkt 268)
                (vector define.rkt/str 88 93)
                "use->pos->name/transitive: prefix-in `plain` part of `PRE:plain`")
  (check-equal? (use-pos->name require.rkt 461)
                (vector require.rkt/str 405 410)
                "use->pos->name: `plain` is from rename-in not from define.rkt")
  (check-equal? (use-pos->name require.rkt 134)
                (vector define.rkt/str 1051 1054)
                "use-pos->name: sub")
  (check-equal? (use-pos->name/transitive require.rkt 134)
                (vector define.rkt/str 958 961)
                "use-pos->name/transitive: sub")
  (check-equal? (use-pos->name require.rkt 138)
                (vector define.rkt/str 1055 1066)
                "use-pos->name: sub/renamed")
  (check-equal? (use-pos->name/transitive require.rkt 138)
                (vector define.rkt/str 1011 1022)
                "use-pos->name/transitive: sub/renamed")
  (check-equal? (use-pos->name require.rkt 175)
                (vector define.rkt/str 1421 1427)
                "use-pos->name: from-m")
  (check-equal? (use-pos->name/transitive require.rkt 175)
                (vector define.rkt/str 1353 1359)
                "use-pos->name: from-m")

  (check-equal? (name-pos->uses/transitive define.rkt 88)
                (list
                 ;;                      def_text use_text use_stx     beg end
                 (vector require.rkt/str "plain"  "plain"  "plain"      42  47)
                 (vector require.rkt/str "plain"  "plain"  "PRE:plain" 268 273)
                 (vector define.rkt/str  "plain"  "plain"  "plain"     109 114)
                 (vector define.rkt/str  "plain"  "plain"  "plain"     138 143))
                "name-pos->uses/transitive: `plain`")
  (check-equal? (name-pos->uses/transitive define.rkt 207)
                (list
                 (vector require.rkt/str "contracted1" "contracted1" "contracted1" 56 67))
                "name-pos->uses/transitive: contracted1")
  (check-equal? (name-pos->uses/transitive define.rkt 367)
                (list
                 (vector require.rkt/str "contracted/renamed" "contracted/renamed" "contracted/renamed" 80 98)
                 (vector require.rkt/str "contracted/renamed" "contracted/renamed" "PRE:contracted/renamed" 294 312)
                 (vector require.rkt/str "contracted/renamed" "contracted/renamed" "contracted/renamed" 433 451))
                "name-pos->uses/transitive: contracted/renamed")
  (check-equal? (name-pos->uses/transitive require.rkt 405)
                (list
                 ;;                      def_text use_text use_stx     beg end
                 (vector require.rkt/str "plain"  "plain"  "plain"     461 466))
                "name-pos->uses/transitive: `plain` from only-in rename")
  (check-equal? (name-pos->uses/transitive require.rkt 452)
                (list
                 ;;                      def_text use_text use_stx     beg end
                 (vector require.rkt/str "c/r"    "c/r"    "c/r"       469 472))
                "name-pos->uses/transitive: `c/r` from only-in rename")
  (check-equal? (name-pos->uses/transitive require.rkt 515)
                (list
                 ;;                      def_text use_text use_stx beg end
                 (vector require.rkt/str "XXX"    "XXX"    "XXX"   524 527))
                "name-pos->uses/transitive: `XXX` from rename-in")

  (check-equal? (name-pos->uses/transitive require.rkt 242)
                (list
                 ;;                      def_text use_text use_stx
                 (vector require.rkt/str "PRE:"   "PRE:"   "PRE:plain"
                         264 268)
                 (vector require.rkt/str "PRE:"   "PRE:"   "PRE:renamed"
                         276 280)
                 (vector require.rkt/str "PRE:"   "PRE:"   "PRE:contracted/renamed"
                         290 294))
                "name-pos->uses/transitive: `PRE:` from prefix-in"))

(module+ test
  (require sql ;for ad hoc queries in REPL
           "create.rkt")
  (open 'memory analyze-code)
  (create-tables)
  (tests))

(module+ on-disk-example
  (require sql ;for ad hoc queries in REPL
           "create.rkt")
  (define-runtime-path db-path "locs.sqlite")
  (create-database db-path)
  (open db-path analyze-code)
  ;; Doing this means analyze-path will queue up more files to
  ;; analyze, transitively, until reaching a fixed point.
  (start-analyze-more-files-thread)
  (tests)

  ;; Re-analyze another file (and watch the `pdb` logger topic)
  (define-runtime-path db.rkt "db.rkt")
  (analyze-path (build-path db.rkt) #:always? #t)

  ;; Do this to refresh everything.
  #;(analyze-all-known-paths #:always? #t)
  )

