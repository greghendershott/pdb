#lang racket/base

(require (for-syntax racket/base)
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
  (phase-tests)
  (space-tests)
  (meta-lang-tests)
  #;(exhaustive-rename-tests)
  )

(define-syntax-parser define-example
  [(_ id:id)
   #`(define-runtime-path id #,(format "example/~a" (syntax->datum #'id)))])

(define-example define.rkt)
(define-example require.rkt)

(define-check (check-set-equal? actual expected)
  (unless (equal? actual expected)
    (define (xf ms) (list->set (set->list ms))) ;mutable->immutable
    (define actual* (xf actual))
    (define expected* (xf expected))
    (define (show s)
      (define n (set-count s))
      (if (<= n 16)
          s
          (string->symbol (format "elided because set has ~v elements" n))))
    (with-check-info
      (['actual     (show actual)]
       ['expected   (show expected)]
       ['missing    (set-subtract expected* actual*)]
       ['unexpected (set-subtract actual* expected*)])
      (fail))))

(define (general-tests)
  (analyze-path (build-path require.rkt) #:always? #t)
  (analyze-path (build-path define.rkt)  #:always? #t)

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

  (check-set-equal? (rename-sites define.rkt 88)
                    (mutable-set
                     (list define.rkt 88 93)
                     (list define.rkt 109 114)
                     (list define.rkt 138 143)
                     (list require.rkt 42 47)
                     (list require.rkt 268 273)
                     (list require.rkt 509 514))
                    "rename-sites: plain")

  (check-set-equal? (rename-sites define.rkt 322)
                    (mutable-set
                     (list define.rkt 322 325)
                     (list define.rkt 363 366))
                    "rename-sites: c/r")

  (check-set-equal? (rename-sites define.rkt 207)
                    (mutable-set
                     (list define.rkt 165 176)
                     (list define.rkt 207 218)
                     (list require.rkt 56 67))
                    "rename-sites: contracted1")

  (check-set-equal? (rename-sites define.rkt 367)
                    (mutable-set
                     (list define.rkt 367 385)
                     (list require.rkt 80 98)
                     (list require.rkt 294 312)
                     (list require.rkt 433 451))
                    "rename-sites: contracted/renamed")

  (check-set-equal? (rename-sites require.rkt 405)
                    (mutable-set
                     (list require.rkt 405 410)
                     (list require.rkt 461 466))
                    "rename-sites: `plain` from only-in rename")

  (check-set-equal? (rename-sites require.rkt 452)
                    (mutable-set
                     (list require.rkt 452 455)
                     (list require.rkt 469 472))
                    "rename-sites: `c/r` from only-in rename")

  (check-set-equal? (rename-sites require.rkt 515)
                    (mutable-set
                     (list require.rkt 515 518)
                     (list require.rkt 524 527))
                    "rename-sites: `XXX` from rename-in")

  (check-set-equal? (rename-sites require.rkt 242)
                    (mutable-set
                     (list require.rkt 242 246)
                     (list require.rkt 264 268)
                     (list require.rkt 276 280)
                     (list require.rkt 290 294))
                    "rename-sites: `PRE:` from prefix-in")

  (check-set-equal? (rename-sites require.rkt 637)
                    (mutable-set
                     (list require.rkt 637 640)
                     (list require.rkt 646 649)
                     (list require.rkt 674 677))
                    "rename-sites: rename-in, use, plus rename-out")

  (check-equal? (use->def/same-name require.rkt 629)
                (list define.rkt 144 151)
                "use->def/same-name: `renamed` as old name in rename-in clause")
  (check-set-equal? (rename-sites require.rkt 48)
                    (mutable-set
                     (list define.rkt 144 151)
                     (list require.rkt 48 55)
                     (list require.rkt 280 287)
                     (list require.rkt 397 404)
                     (list require.rkt 629 636))
                    "rename-sites: `renamed`")

  (check-set-equal? (rename-sites require.rkt 753)
                    (mutable-set
                     (list require.rkt 134 137)
                     (list require.rkt 753 756)
                     (list define.rkt 958 961)
                     (list define.rkt 979 982)
                     (list define.rkt 1007 1010)
                     (list define.rkt 1051 1054))
                    "rename-sites: def->uses/same-name handles non-null submods"))

(define-example define-foo.rkt)
(define-example define-bar.rkt)
(define-example re-provide.rkt)
(define-example require-re-provide.rkt)

(define (re-provide-tests)
  (analyze-path (build-path define-foo.rkt) #:always? #t)
  (analyze-path (build-path define-bar.rkt) #:always? #t)
  (analyze-path (build-path re-provide.rkt) #:always? #t)
  (analyze-path (build-path require-re-provide.rkt) #:always? #t)
  (check-equal? (use->def require-re-provide.rkt 41)
                (list define-foo.rkt 36 39)
                "use->def: foo")
  (check-equal? (nominal-use->def require-re-provide.rkt 41)
                (list define-foo.rkt 23 26)
                "nominal-use->def: foo [all-from-out]")
  (check-set-equal? (rename-sites define-foo.rkt 36)
                    (mutable-set
                     (list define-foo.rkt 36 39)
                     (list define-foo.rkt 23 26)
                     (list require-re-provide.rkt 41 44))
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
  (check-set-equal? (rename-sites define-bar.rkt 36)
                    (mutable-set
                     (list define-bar.rkt 36 39)
                     (list define-bar.rkt 23 26)
                     (list re-provide.rkt 119 122)
                     (list require-re-provide.rkt 45 48))
                    "rename-sites: bar"))

(define-example ado-define.rkt)
(define-example ado-require.rkt)

(define (all-defined-out-tests)
  (analyze-path (build-path ado-define.rkt) #:always? #t)
  (analyze-path (build-path ado-require.rkt) #:always? #t)
  (check-equal? (use->def ado-require.rkt 46)
                (list ado-define.rkt 27 28)))

(define-example phase/single.rkt)
(define-example phase/define.rkt)
(define-example phase/require.rkt)

(define (phase-tests)
  (analyze-path (build-path phase/single.rkt) #:always? #t)
  (check-equal? (use->def phase/single.rkt 233)
                (list phase/single.rkt 125 126)
                "phase 0 use-pos->def")
  (check-equal? (use->def phase/single.rkt 276)
                (list phase/single.rkt 177 178)
                "phase 1 use-pos->def")

  (analyze-path (build-path phase/define.rkt) #:always? #t)
  (analyze-path (build-path phase/require.rkt) #:always? #t)
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
  (analyze-path (build-path space/define.rkt) #:always? #t)
  (analyze-path (build-path space/require.rkt) #:always? #t)
  (test-case "phase 0 space #f things still work"
    (check-equal? (use->def space/require.rkt 70)
                  (list space/define.rkt 312 318)
                  "phase 0 space #f use->def")
    (check-equal? (def->uses/same-name space/define.rkt 312)
                  (mutable-set
                   (list space/define.rkt 97 103)
                   (list space/require.rkt 70 76))
                  "phase 0 space #f def->uses/same-name"))
  (test-case "non-#f spaces"
    ;; TODO: More tests involving spaces --- but it looks like
    ;; drracket/check-syntax isn't drawing arrows for these, yet, so
    ;; not sure what to test, yet.
    ))

(define-example meta-lang.rkt)

(define (meta-lang-tests)
  (analyze-path (build-path meta-lang.rkt) #:always? #t)
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
  (define a (interval-map-ref (file-arrows (hash-ref files meta-lang.rkt)) 27))
  (check-true (import-arrow? a))
  (check-equal? (arrow-def-beg a) 14)
  (check-equal? (arrow-def-sym a) 'racket/base))

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
  (printf "~v ..." (list 'exhaustive-rename-sites-test path))

  (define ht (make-hash))
  (define (rename-sites/memo path pos)
    (hash-ref! ht
               (cons path pos)
               (λ () (rename-sites path pos))))

  (define len (add1 (file-size path)))
  (let loop ([pos 1]
             [previous-results #f])
    (when (< pos len)
      (printf "~v::~v\n" path pos)
      (define results (rename-sites/memo path pos))
      (when (not (equal? results previous-results))
        (printf " checking ~v results\n" (set-count results))
        (for ([loc (in-set results)])
          (match-define (list this-path this-pos _) loc)
          (unless (and (equal? path this-path)
                       (equal? pos this-pos))
            (printf "  ~v::~v\n" this-path this-pos)
            (check-set-equal? (rename-sites/memo this-path this-pos)
                              results
                              (format "<~v ~v> <~v ~v>"
                                      path pos
                                      this-path this-pos)))))
      (loop (add1 pos)
            results)))
  (newline))

(define (exhaustive-rename-tests)
  (for-each exhaustive-rename-sites-test
            (list require.rkt
                  define.rkt ;quite slow!
                  require-re-provide.rkt)))

(module+ test
  (tests))

(module+ on-disk-example
  (define starting-memory-use (current-memory-use))

  (define-runtime-path db-path (build-path "example" "test.rktd.gz"))
  (time (load db-path))

  ;; Re-analyze another file (and watch the `pdb` logger topic). Here
  ;; we use #:always #t to force analysis regardless of whether the
  ;; file has changed.
  (define-runtime-path main.rkt "main.rkt")
  (analyze-path (build-path main.rkt) #:always? #t)

  ;; Do this to queue for analysis an entire directory tree.
  (queue-directory-to-analyze
   (string->path "/home/greg/src/racket-lang/racket/collects/"))

  ;; Do this to analyze all files discovered. With #:always? #f each
  ;; file will be fully re-analyzed only if its digest is invalid (if
  ;; the file has changed, or, the digest was deleted to force a
  ;; fresh analysis).
  (time (analyze-all-known-paths #:always? #f))
  (printf "~v MB memory use, ~v files \n"
          (/ (- (current-memory-use) starting-memory-use)
             1024.0
             1024.0)
          (hash-count files))

  (time (save db-path))

  ;; Do this to refresh everything from scratch. (But if you change
  ;; the schema, just delete the .rktd file.)
  #;(time (analyze-all-known-paths #:always? #t))

  (tests))
