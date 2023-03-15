#lang racket/base

(require (for-syntax racket/base)
         pkg/path
         racket/format
         racket/match
         racket/runtime-path
         racket/set
         rackunit
         syntax/parse/define
         "main.rkt"
         (submod "main.rkt" private))

(define (tests)
  (general-tests)
  (re-provide-tests)
  (all-defined-out-tests)
  (prefix-tests)
  (phase-tests)
  (space-tests)
  (meta-lang-tests)
  (typed-tests)
  (error-tests)
  (exhaustive-rename-tests))

(define-runtime-path example-path "example/")

(define-syntax-parser define-example
  [(_ id:id)
   #`(define-runtime-path id #,(format "example/~a" (syntax->datum #'id)))])

(define-example define.rkt)
(define-example require.rkt)

(define (general-tests)
  (analyze-path require.rkt #:always? #t)
  (analyze-path define.rkt  #:always? #t)

  ;;; uses -> definitions
  ;;;
  ;;; use->def: Big jumps directly to definition
  ;;; nominal-use->def: Incremental steps

  (check-equal? (use->def require.rkt 42)
                (list define.rkt 88 93)
                "use->def: plain")

  (check-equal? (use->def require.rkt 48)
                (list define.rkt 88 93)
                "use->def: renamed")

  (check-equal? (use->def require.rkt 56)
                (list define.rkt 165 176)
                "use->def: contracted1")
  (check-equal? (nominal-use->def require.rkt 56)
                (list define.rkt 207 218)
                "nominal-use->def: contracted1")

  (check-equal? (use->def require.rkt 68)
                (list define.rkt 246 257)
                "use->def: contracted2")
  (check-equal? (nominal-use->def require.rkt 68)
                (list define.rkt 283 294)
                "nominal-use->def: contracted2")

  (check-equal? (use->def require.rkt 80)
                (list define.rkt 322 325)
                "use->def: contracted/renamed => c/r")
  (check-equal? (nominal-use->def require.rkt 80)
                (list define.rkt 367 385)
                "nominal-use->def: contracted/renamed => contracted/renamed in rename clause")
  (check-equal? (nominal-use->def define.rkt 367)
                (list define.rkt 363 366)
                "nominal-use->def: contracted/renamed => c/r within rename clause")
  (check-equal? (nominal-use->def define.rkt 363)
                (list define.rkt 322 325)
                "nominal-use->def: c/r within rename clause => c/r definition")

  (check-equal? (use->def require.rkt 99)
                (list define.rkt 515 529)
                "use->def: plain-by-macro")
  (check-equal? (use->def require.rkt 114)
                (list define.rkt 684 703)
                "use->def: contracted-by-macro")
  (check-equal? (use->def require.rkt 134)
                (list define.rkt 958 961)
                "use->def: sub")
  (check-equal? (use->def require.rkt 138)
                (list define.rkt 958 961)
                "use->def: sub/renamed")
  (check-equal? (use->def require.rkt 150)
                (list define.rkt 1179 1182)
                "use->def: foo")
  (check-equal? (use->def require.rkt 154)
                (list define.rkt 1225 1233)
                "use->def: a-number")
  (check-equal? (use->def require.rkt 163)
                (list define.rkt 1265 1276)
                "use->def: a-parameter")

  (check-equal? (use->def require.rkt 175)
                (list define.rkt 1353 1359)
                "use->def: from-m")
  (check-equal? (nominal-use->def require.rkt 175)
                (list define.rkt 1421 1427)
                "nominal-use->def: from-m in contract-out provide")
  #;
  (check-equal? (nominal-use->def define.rkt 1421)
                (list define.rkt 1375 1381)
                "nominal-use->def: from-m in contract-out provide to submod plain provide; but check-syntax is drawing this direct to the definition, skipping the submod provide :(")
  (check-equal? (nominal-use->def define.rkt 1375)
                (list define.rkt 1353 1359)
                "nominal-use->def: from-m in contract-out provide to submod plain provide")

  (check-equal? (use->def require.rkt 182)
                (list define.rkt 1456 1459)
                "use->def: d/c")
  (check-equal? (use->def require.rkt 186)
                (list define.rkt 1456 1459)
                "use->def: renamed-d/c")

  (check-equal? (use->def require.rkt 405)
                (list define.rkt 88 93)
                "use->def: arrow for only-in rename")
  (check-equal? (nominal-use->def require.rkt 405)
                (list require.rkt 397 404)
                "nominal-use->def: arrow for only-in rename; needs PR in Racket 8.1.0.3")

  (check-equal? (use->def require.rkt 452)
                (list define.rkt 322 325)
                "use->def: arrow for only-in rename")
  (check-equal? (nominal-use->def require.rkt 452)
                (list require.rkt 433 451)
                "nominal-use->def: arrow for only-in rename; needs PR to Racket 8.1.0.3")

  (check-equal? (use->def require.rkt 529)
                (list define.rkt 1545 1553)
                "use->def: a-struct")
  (check-equal? (use->def require.rkt 538)
                (list define.rkt 1545 1553)
                "use->def: a-struct?")
  (check-equal? (use->def require.rkt 548)
                (list define.rkt 1545 1553)
                "use->def: sub-range `a-struct` of imported `a-struct-a`")
  (check-equal? (use->def require.rkt 557)
                (list define.rkt 1555 1556)
                "use->def: sub-range `a`  of imported `a-struct-a`")
  (check-equal? (use->def require.rkt 559)
                (list define.rkt 1545 1553)
                "use->def: imported a-struct-b")
  (check-equal? (use->def define.rkt 1593)
                (list define.rkt 1545 1553)
                "use->def: local a-struct portion of a-struct-a")
  (check-equal? (use->def define.rkt 1602)
                (list define.rkt 1555 1556)
                "use->def: local 'a' field portion of a-struct-a")

  (check-equal? (nominal-use->def require.rkt 42)
                (list define.rkt 109 114)
                "nominal-use->def plain")
  (check-equal? (use->def require.rkt 42)
                (list define.rkt 88 93)
                "use->def: plain")
  (check-equal? (nominal-use->def require.rkt 48)
                (list define.rkt 144 151)
                "nominal-use->def: renamed")
  (check-equal? (nominal-use->def require.rkt 264)
                (list require.rkt 242 246)
                "use->pos->name: prefix-in `PRE:` part of `PRE:plain`")
  (check-equal? (nominal-use->def require.rkt 268)
                (list define.rkt 109 114)
                "use->pos->name: prefix-in `plain` part of `PRE:plain`")
  (check-equal? (use->def require.rkt 268)
                (list define.rkt 88 93)
                "use->pos->name/transitive: prefix-in `plain` part of `PRE:plain`")
  (check-equal? (nominal-use->def require.rkt 461)
                (list require.rkt 405 410)
                "use->pos->name: `plain` is from rename-in not from define.rkt")

  (check-equal? (use->def require.rkt 134)
                (list define.rkt 958 961)
                "use->def: `sub`")
  (check-equal? (nominal-use->def require.rkt 134)
                (list define.rkt 1051 1054)
                "nominal-use->def: outermost provide of `sub`")
  #;
  (check-equal? (nominal-use->def define.rkt 1051)
                (list define.rkt 979 982)
                "nominal-use->def: outermost provide of `sub` => submod provide; check-syntax arrow bug???")
  (check-equal? (nominal-use->def define.rkt 979)
                (list define.rkt 958 961)
                "nominal-use->def: submod provide of `sub` to definition")

  (check-equal? (use->def require.rkt 138)
                (list define.rkt 958 961)
                "use->def: sub/renamed")
  (check-equal? (nominal-use->def require.rkt 138)
                (list define.rkt 1055 1066)
                "nominal-use->def: outermost provide of sub/renamed")

  (check-equal? (nominal-use->def require.rkt 529)
                (list define.rkt 1545 1553)
                "nominal-use->def: a-struct")
  (check-equal? (nominal-use->def require.rkt 538)
                (list define.rkt 1545 1553)
                "nominal-use->def: a-struct?")
  (check-equal? (nominal-use->def require.rkt 548)
                (list define.rkt 1545 1553)
                "nominal-use->def: imported a-struct-a")
  (check-equal? (nominal-use->def require.rkt 557)
                (list define.rkt 1555 1556)
                "nominal-use->def: imported 'a' field portion of a-struct-a")
  (check-equal? (nominal-use->def require.rkt 559)
                (list define.rkt 1545 1553)
                "nominal-use->def: imported a-struct-b")
  (check-equal? (nominal-use->def define.rkt 1593)
                (list define.rkt 1545 1553)
                "nominal-use->def: local a-struct portion of a-struct-a")
  (check-equal? (use->def define.rkt 1602)
                (list define.rkt 1555 1556)
                "nominal-use->def: local 'a' field portion of a-struct-a")

  ;;; rename-sites

  (check-equal? (rename-sites define.rkt 88)
                (hash define.rkt '((88 . 93) (109 . 114) (138 . 143))
                      require.rkt '((42 . 47) (268 . 273) (509 . 514)))
                "rename-sites: plain")

  (check-equal? (rename-sites define.rkt 322)
                (hash define.rkt '((322 . 325) (363 . 366)))
                "rename-sites: c/r")

  (check-equal? (rename-sites define.rkt 207)
                (hash define.rkt '((165 . 176) (207 . 218))
                      require.rkt '((56 . 67)))
                "rename-sites: contracted1")

  (check-equal? (rename-sites define.rkt 367)
                (hash define.rkt '((367 . 385))
                      require.rkt '((80 . 98) (294 . 312) (433 . 451)))
                "rename-sites: contracted/renamed")

  (check-equal? (rename-sites require.rkt 405)
                (hash require.rkt '((405 . 410) (461 . 466)))
                "rename-sites: `plain` from only-in rename")

  (check-equal? (rename-sites require.rkt 452)
                (hash require.rkt '((452 . 455) (469 . 472)))
                "rename-sites: `c/r` from only-in rename")

  (check-equal? (rename-sites require.rkt 515)
                (hash require.rkt '((515 . 518) (524 . 527)))
                "rename-sites: `XXX` from rename-in")

  (check-equal? (rename-sites require.rkt 242)
                (hash require.rkt '((242 . 246) (264 . 268) (276 . 280) (290 . 294)))
                "rename-sites: `PRE:` from prefix-in")

  (check-equal? (rename-sites require.rkt 637)
                (hash require.rkt '((637 . 640) (646 . 649) (674 . 677)))
                "rename-sites: rename-in, use, plus rename-out")

  (check-equal? (use->def/same-name require.rkt 629)
                (list define.rkt 144 151)
                "use->def/same-name: `renamed` as old name in rename-in clause")
  (check-equal? (rename-sites require.rkt 48)
                (hash define.rkt '((144 . 151))
                      require.rkt '((48 . 55) (280 . 287) (397 . 404) (629 . 636)))
                "rename-sites: `renamed`")

  (check-equal? (rename-sites require.rkt 753)
                (hash require.rkt '((134 . 137) (753 . 756))
                      define.rkt '((958 . 961) (979 . 982) (1007 . 1010) (1051 . 1054)))
                "rename-sites: rename-sites handles non-null submods"))

(define-example define-foo.rkt)
(define-example define-bar.rkt)
(define-example re-provide.rkt)
(define-example require-re-provide.rkt)

(define (re-provide-tests)
  (analyze-path define-foo.rkt #:always? #t)
  (analyze-path define-bar.rkt #:always? #t)
  (analyze-path re-provide.rkt #:always? #t)
  (analyze-path require-re-provide.rkt #:always? #t)
  (check-equal? (use->def require-re-provide.rkt 41)
                (list define-foo.rkt 36 39)
                "use->def: foo")
  (check-equal? (nominal-use->def require-re-provide.rkt 41)
                (list define-foo.rkt 23 26)
                "nominal-use->def: foo [all-from-out]")
  (check-equal? (rename-sites define-foo.rkt 36)
                (hash define-foo.rkt '((23 . 26) (36 . 39))
                      require-re-provide.rkt '((41 . 44)))
                "rename-sites: foo")

  (check-equal? (use->def require-re-provide.rkt 45)
                (list define-bar.rkt 36 39)
                "use->def: bar")
  (check-equal? (nominal-use->def require-re-provide.rkt 45)
                (list re-provide.rkt 119 122)
                "nominal-use->def: bar")
  (check-equal? (use->def require-re-provide.rkt 45)
                (list define-bar.rkt 36 39)
                "nominal-use->def: bar")
  (check-equal? (rename-sites define-bar.rkt 36)
                (hash define-bar.rkt '((23 . 26) (36 . 39))
                      re-provide.rkt '((119 . 122))
                      require-re-provide.rkt '((45 . 48)))
                "rename-sites: bar"))

(define-example ado-define.rkt)
(define-example ado-require.rkt)

(define (all-defined-out-tests)
  (analyze-path ado-define.rkt #:always? #t)
  (analyze-path ado-require.rkt #:always? #t)
  (check-equal? (use->def ado-require.rkt 46)
                (list ado-define.rkt 35 36))
  (check-equal? (rename-sites ado-require.rkt 46)
                (hash ado-require.rkt '((46 . 47))
                      ado-define.rkt '((35 . 36)))
                "all-defined-out: rename-sites for a"))

(define-example prefix-define.rkt)
(define-example prefix-require.rkt)

(define (prefix-tests)
  (analyze-path prefix-define.rkt #:always? #t)
  (analyze-path prefix-require.rkt #:always? #t)
  (check-equal? (rename-sites prefix-require.rkt 68) ;(prefix-in IN: "prefix-define.rkt")
                (hash prefix-require.rkt
                      '((68 . 71) ;(prefix-in IN: "prefix-define.rkt")
                        (110 . 113) ;IN:A:a
                        (117 . 120) ;IN:ALL:a
                        (126 . 129))) ;IN:ALL:b
                "rename-sites for prefix-in IN:")
  ;; Following are two tests I want to write, but which don't pass. :(
  ;; Why, IIUC: 1. prefix-out does not support sub-range-binders. 2.
  ;; (all-defined-out) is the srcloc for all defs. In other words I
  ;; don't think we can make these tests pass unless prefix-out and
  ;; all-defined-out are changed in Racket itself.
  #;
  (check-equal? (rename-sites prefix-define.rkt 27) ;(define a 42)
                (hash prefix-define.rkt
                      '((27 . 28) ;(define a 42)
                        (71 . 72)) ;(prefix-out A: a)
                      prefix-require.rkt
                      '((96 . 97) ;A:a
                        (102 . 103) ;ALL:a
                        (115 . 116) ;IN:A:a
                        (124 . 125))) ;IN:ALL:a
                "rename-sites for a")
  #;
  (check-equal? (rename-sites prefix-define.rkt 68) ;(prefix-out A: a)
                (hash prefix-define.rkt
                      '((68 . 70)) ;(prefix-out A: a)
                      prefix-require.rkt
                      '((94 . 96) ;A:a
                        (113 . 115))) ;IN:A:a
                "rename-sites for prefix-out A:"))

(define-example phase/single.rkt)
(define-example phase/define.rkt)
(define-example phase/require.rkt)

(define (phase-tests)
  (analyze-path phase/single.rkt #:always? #t)
  (check-equal? (use->def phase/single.rkt 233)
                (list phase/single.rkt 125 126)
                "phase 0 use-pos->def")
  (check-equal? (use->def phase/single.rkt 276)
                (list phase/single.rkt 177 178)
                "phase 1 use-pos->def")

  (analyze-path phase/define.rkt #:always? #t)
  (analyze-path phase/require.rkt #:always? #t)
  (check-equal? (use->def phase/require.rkt 97)
                (list phase/define.rkt 64 65)
                "phase 0 use-pos->def")
  (check-equal? (use->def phase/require.rkt 140)
                (list phase/define.rkt 110 111)
                "phase 1 use-pos->def")

  (check-equal? (nominal-use->def phase/require.rkt 97)
                (list phase/define.rkt 78 79)
                "phase 0 nominal-use->def")
  (check-equal? (use->def phase/require.rkt 97)
                (list phase/define.rkt 64 65)
                "phase 0 use->def")
  (check-equal? (nominal-use->def phase/require.rkt 140)
                (list phase/define.rkt 126 127)
                "phase 1 nominal-use->def")
  (check-equal? (use->def phase/require.rkt 140)
                (list phase/define.rkt 110 111)
                "phase 1 use->def")
  (check-equal? (nominal-use->def phase/require.rkt 164)
                (list phase/define.rkt 154 167)
                "phase 1 nominal-use->def rename-out"))

(define-example space/define.rkt)
(define-example space/require.rkt)

(define (space-tests)
  (analyze-path space/define.rkt #:always? #t)
  (analyze-path space/require.rkt #:always? #t)
  (test-case "phase 0 space #f things still work"
    (check-equal? (use->def space/require.rkt 70)
                  (list space/define.rkt 312 318)
                  "phase 0 space #f use->def")
    (check-equal? (rename-sites space/define.rkt 312)
                  (hash space/define.rkt '((97 . 103) (312 . 318))
                        space/require.rkt '((70 . 76)))
                  "phase 0 space #f rename-sites"))
  (test-case "non-#f spaces"
    ;; TODO: More tests involving spaces --- but it looks like
    ;; drracket/check-syntax isn't drawing arrows for these, yet, so
    ;; not sure what to test, yet.
    ))

(define-example meta-lang.rkt)

(define (meta-lang-tests)
  (analyze-path meta-lang.rkt #:always? #t)
  ;; The following test will pass only if
  ;;
  ;;   <https://github.com/racket/racket/pull/3902>
  ;;
  ;; is merged to change `make-meta-reader` to address
  ;;
  ;;   <https://github.com/racket/drracket/issues/486>
  ;;
  ;; Here we assume it will be merged sometime after the now-current
  ;; version 8.2.0.1 of Racket as built from source.
  (define a (interval-map-ref (file-arrows (get-file meta-lang.rkt)) 27))
  (check-true (import-arrow? a))
  (check-equal? (arrow-def-beg a) 14))

(define-example typed.rkt)
(define-example typed-error.rkt)

(define (typed-tests)
  (analyze-path typed.rkt #:always? #t)
  (check-equal? (span-map->list (file-mouse-overs (get-file typed.rkt)))
                '(((7 . 24) "7 bound occurrences")
                  ((26 . 27) "(-> Number Number)")
                  ((27 . 33) "imported from typed/racket/base")
                  ((35 . 36) "no bound occurrences")
                  ((38 . 39) "1 bound occurrence")
                  ((53 . 54) "imported from typed/racket/base" "Number")
                  ((54 . 55) "imported from typed/racket/base" "(-> Number * Number)")
                  ((56 . 57) "Number")
                  ((58 . 59) "imported from typed/racket/base" "One")
                  ((59 . 60) "Number")
                  ((60 . 61) "(-> Number Number)"))
                "Typed Racket mouse-overs from online-check-syntax logger")

  (analyze-path typed-error.rkt #:always? #t)
  (check-equal? (span-map->list (file-errors (get-file typed-error.rkt)))
                `(((45 . 46) (#f . ,(~a typed-error.rkt
                                        ":4:5: Type Checker: type mismatch\n  expected: Number\n  given: Any\n  in: x")))
                  ((71 . 72) (#f . ,(~a typed-error.rkt
                                        ":7:5: Type Checker: type mismatch\n  expected: Number\n  given: Any\n  in: x"))))
                "Typed Racket: multiple pre-exn errors gathered")
  (check-equal? (span-map->list (file-mouse-overs (get-file typed-error.rkt)))
                '(((26 . 27) "(-> Any Nothing)")
                  ((43 . 44) "(-> Number * Number)")
                  ((45 . 46) "type mismatch\n  expected: Number\n  given: Any")
                  ((47 . 48) "One")
                  ((49 . 50) "(-> Any Nothing)")
                  ((52 . 53) "(-> Any Nothing)")
                  ((69 . 70) "(-> Number * Number)")
                  ((71 . 72) "type mismatch\n  expected: Number\n  given: Any")
                  ((73 . 74) "One")
                  ((75 . 76) "(-> Any Nothing)"))
                "Typed Racket error: mouse-overs from online-check-syntax logger"))

(define-example error.rkt)
(define-example require-error.rkt)

(define (error-tests)
  (analyze-path require-error.rkt #:always? #t)
  (check-true
   (match (span-map->list (file-errors (get-file require-error.rkt)))
     [(list (list (cons 28 35)
                  (cons (== error.rkt)
                        ;; This message has many system-dependent
                        ;; paths so not checking it here in detail.
                        (? string?))))
      #t]
     [_ #f])
   "Error in imported file is correctly recorded."))

;; Test that, for every file position, the rename-site results set is
;; identical when rename-sites is called for every position in that
;; result set. IOW calling rename-sites for /any/ of them always
;; returns /all/ of them.
;;
;; Note that this test can fail unless /all/ relevant files have been
;; analyzed, so that all uses are known. As one example, two files may
;; use `define` as imported via #lang racket vs. #lang racket/base,
;; and as a result might give different results if the racket/base
;; graph has been discovered but not the racket graph. This is
;; complicated by us analyzing files JIT. So the most reliable way to
;; use this test is to use queue-directory-to-analyze, then
;; analyze-all-known-files, prior to running the test.
;;
;; Finally, this test is _extremely_ slow when run on a full analysis
;; of hundreds of files, such as the racket/collects tree.
;;
;; TL;DR: This isn't a very good test, but it did help me find and fix
;; a problem with anonymous re-provides, when run on just the
;; example/*.rkt files.
(define (exhaustive-rename-sites-test path)
  (printf "~a ..." (list 'exhaustive-rename-sites-test path))

  (define ht (make-hash))
  (define (rename-sites/memo path pos)
    (hash-ref! ht
               (cons path pos)
               (λ () (rename-sites path pos))))

  (define len (add1 (file-size path)))
  (let loop ([pos 1]
             [previous-results #f])
    (when (< pos len)
      ;;(printf "~v::~v\n" path pos)
      (define results (rename-sites/memo path pos))
      (when (not (equal? results previous-results))
        (for ([(this-path locs) (in-hash results)])
          ;;(printf " checking ~v locs in ~v\n" (set-count locs) this-path)
          (for ([loc (in-set locs)])
            (match-define (cons this-pos _) loc)
            (unless (and (equal? path this-path)
                         (equal? pos this-pos))
              ;;(printf "  ~v::~v\n" this-path this-pos)
              (check-equal? (rename-sites/memo this-path this-pos)
                            results
                            (format "<~v ~v> <~v ~v>"
                                    path pos
                                    this-path this-pos))))))
      (loop (add1 pos)
            results)))
  (newline))

(define (exhaustive-rename-tests)
  (for-each exhaustive-rename-sites-test
            (list require.rkt
                  define.rkt ;quite slow!
                  require-re-provide.rkt)))

(module+ test
  (open)
  (tests)
  (close))

(module+ very-many-files-example
  (define starting-memory-use (current-memory-use))

  (open)

  ;; Re-analyze another file (and watch the `pdb` logger topic). Here
  ;; we use #:always #t to force analysis regardless of whether the
  ;; file has changed.
  (define-runtime-path main.rkt "main.rkt")
  (analyze-path (build-path main.rkt) #:always? #t)

  ;; Use `queue-directory-to-analyze' to queue for analysis some
  ;; entire directory trees.
  ;;
  ;; On my system -- with the non-minimal Racket distribution
  ;; installed, and about a dozen other packages -- this results in
  ;; about 8,000 files, which takes nearly 3 hours to analyze,
  ;; and yields a 92 MiB pdb-main.sqlite file.
  (for ([d (in-list (list* (get-pkgs-dir 'installation)
                           (get-pkgs-dir 'user)
                           (current-library-collection-paths)))])
    (when (directory-exists? d)
      (queue-directory-to-analyze d)))

  ;; Do this to analyze all files discovered. With #:always? #f each
  ;; file will be fully re-analyzed only if its digest is invalid (if
  ;; the file has changed, or, the digest was deleted to force a
  ;; fresh analysis).
  (time (analyze-all-known-paths #:always? #f))
  (printf "~v MB memory use, ~v files \n"
          (/ (- (current-memory-use) starting-memory-use)
             1024.0
             1024.0)
          (length (all-known-paths)))

  ;; Do this to refresh everything from scratch. (But if you change
  ;; the schema, just delete the .rktd file.)
  #;(time (analyze-all-known-paths #:always? #t))

  (tests)

  (analyze-all-known-paths)
  (close))
