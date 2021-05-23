#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (prefix-in db: db)
         racket/contract
         sql
         "db.rkt"
         "common.rkt")

(provide create-database
         create-tables)

(define/contract (create-database path)
  (-> path-string? any)
  (unless (file-exists? path)
    (log-pdb-warning "~v does not exist; creating it and tables" path)
    (parameterize ([current-dbc (db:sqlite3-connect #:database  path
                                                    #:mode      'create
                                                    #:use-place #f)])
      (create-tables)
      (db:disconnect (current-dbc)))))

;; This "DRYs" creating tables -- and convenience companion views --
;; that use "interned" string columns; see `strings` table comment
;; below. This expands to a `create-table` where the foreign-key
;; constraints are automatically supplied, as well as a `create-view`
;; -- using the table name with a "_view" suffix -- where the interned
;; strings are selected.
(define-syntax (create-table/interned stx)
  (define-syntax-class column-spec
    #:attributes (table view (fk 1))
    (pattern [?id:id (~datum string)]
             #:with table    #'[?id integer #:not-null]
             #:with view     #'(select str #:from strings #:where (= ?id strings.id))
             #:with (fk ...) #'((foreign-key ?id #:references (strings id))))
    (pattern [?id:id other:id]
             #:with table    #'[?id other #:not-null]
             #:with view     #'?id
             #:with (fk ...) #'()))
  (syntax-parse stx
    [(_ table-name:id
        #:columns column-spec:column-spec ...+
        #:constraints . more)
     #:with view-name (format-id #'table-name "~a_view" #'table-name)
     #'(begin
         (query-exec
          (create-table #:if-not-exists table-name
                        #:columns column-spec.table ...
                        #:constraints column-spec.fk ... ... . more))
         (query-exec
          (create-view view-name
                       (select column-spec.view ... #:from table-name))))]))

(define (create-tables)
  ;; The `strings` table is like interned symbols. There are many
  ;; long, repeated strings -- such as for paths, submods,
  ;; identifiers. These can be replaced by integer foreign keys.
  ;;
  ;; Saves much space. Also speeds queries that test for equality.
  ;; Although it complicates some queries that need the string values,
  ;; by requiring table join(s), the "ON" clauses are cheap integer
  ;; equality.
  ;;
  ;; (Although this might seem similar also to a "snowflake schema" in
  ;; a data warehouse, strictly that would be if we had a table for
  ;; each kind of thing a.k.a. "dimension". Instead this is one shared
  ;; table; really just interning. Even so, it does makes tables like
  ;; `def_arrows` and `defs` look a lot like data warehouse "fact"
  ;; tables, whose columns are all integers, many of which are foreign
  ;; keys into `strings`.)
  (query-exec
   (create-table
    #:if-not-exists strings
    #:columns
    [id          integer #:not-null]
    [str         string  #:not-null]
    #:constraints
    (primary-key id)
    (unique      str)))

  ;; This table of sha-1 digests for paths is how we record whether to
  ;; bother re-analyzing a file.
  (query-exec
   (create-table
    #:if-not-exists digests
    #:columns
    [path        integer #:not-null]
    [digest      string  #:not-null]
    #:constraints
    (primary-key path)
    (foreign-key path #:references (strings id))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Miscellaneous

  ;; Tables that simply store values from syncheck-annotations<%>.

  ;; A table of imports. This is useful for completion candidates --
  ;; symbols that could be used, even if they're not yet (and
  ;; therefore don't have any arrow).
  (create-table/interned
   imports
   #:columns
   [path        string]
   [subs        string]
   [sym         string]
   #:constraints
   (primary-key path subs sym))

  ;; A table of syncheck:add-mouse-over-status annotations
  (create-table/interned
   mouseovers
   #:columns
   [path        string]
   [beg         integer]
   [end         integer]
   [text        string]
   #:constraints
   (check       (< 0 beg))
   (check       (< 0 end))
   (check       (< beg end)) ;half-open interval
   (unique      path beg end text))

  ;; A table of syncheck:add-tail-arrow annotations
  (create-table/interned
   tail_arrows
   #:columns
   [path        string]
   [tail        integer]
   [head        integer]
   #:constraints
   (check       (< 0 tail))
   (check       (< 0 head))
   (unique      path tail head))

  ;; A table of syncheck:add-unused-require annotations
  (create-table/interned
   unused_requires
   #:columns
   [path        integer]
   [beg         integer]
   [end         integer]
   #:constraints
   (check       (< 0 beg))
   (check       (< 0 end))
   (check       (< beg end)) ;half-open interval
   (unique      path beg end))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Sub-range-binders. This is used by both the definition and name
  ;; graphs. Each row is similar to a single vector in a
  ;; sub-range-binders property value. So for instance there will be
  ;; rows for both <a-b a> and <a-b b>.
  (create-table/interned
   sub_range_binders
   #:columns
   [path        string]
   [subs        string]
   [full_id     string]
   [sub_ofs     integer]
   [sub_span    integer]
   [sub_id      string]
   [sub_beg     integer]
   [sub_end     integer]
   #:constraints
   (primary-key path subs full_id sub_ofs sub_span)
   (check       (<= 0 sub_ofs))
   (check       (< 0 sub_span))
   (check       (< 0 sub_beg))
   (check       (< 0 sub_end))
   (check       (< sub_beg sub_end))) ;half-open interval

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Definition graph

  ;; A table of arrows, both lexical and imported, as reported by
  ;; syncheck:add-arrow. In addition when the definition is imported
  ;; -- when `kind` is not "lexical" -- it also includes information
  ;; from identifier-binding -- a superset of what
  ;; syncheck:add-jump-to-definition supplies -- about the modpath
  ;; where it is defined. The precise location within that other file
  ;; can be found in `defs` after that file is also analyzed.
  (create-table/interned
   def_arrows
   #:columns
   [use_path    string]
   [use_beg     integer]
   [use_end     integer]
   ;; `use_text` is the annotated text at the use site, i.e. it is
   ;; the [use_beg use_end) interval of use_path. It can be a
   ;; substring of the `use_stx` column, in the case of multiple
   ;; arrows for sub-sections of one identifier, arising from e.g.
   ;; prefix-in or the 'sub-range-binders syntax property.
   [use_text    string]
   [use_stx     string]
   ;; One of {0="lexical" 1="require" 2="module-lang"}
   [kind        integer]
   ;; When `kind` is 0 ("lexical"), this is the local definition
   ;; site. Otherwise, this is the require site.
   [def_beg     integer]
   [def_end     integer]
   [def_text    string] ;text at def site
   [def_stx     string] ;is this ever useful??
   ;; Unless kind is 0 ("lexical"), these correspond to
   ;; identifier-binding from-xxx items. Specifically, join these on
   ;; the `defs` table to find the location within the file, if
   ;; already known. When kind="lexical", only from_path is
   ;; meaningful and is simply the same as use_path.
   [from_path   string] ;from-mod path
   [from_subs   string] ;from-mod subs
   [from_id     string] ;from-id
   #:constraints
   (primary-key use_path use_beg use_end)
   (check       (in kind #:values 0 1 2))
   (check       (< 0 use_beg))
   (check       (< 0 use_end))
   (check       (< use_beg use_end)) ;half-open interval
   (check       (< 0 def_beg))
   (check       (< 0 def_end))
   (check       (< def_beg def_end))) ;half-open interval
  ;; TODO: Add indexes, e.g. for [def_beg def_end] columns?

  ;; A table of definitions in files, as reported by
  ;; syncheck:add-definition-target. Note that this does not include
  ;; sites of lexical definitions.
  (create-table/interned
   defs
   #:columns
   ;; Each definition is uniquely identified by -- i.e. the primary
   ;; key consists of -- these columns:
   [from_path   string]
   [from_subs   string]
   [from_id     string]
   ;; Otherwise we just record the [beg end) location within the
   ;; file.
   [beg         integer]
   [end         integer]
   #:constraints
   (primary-key from_path from_subs from_id)
   (check       (< 0 beg))
   (check       (< 0 end))
   (check       (< beg end))) ;half-open interval

  ;; This view joins `defs` and `sub_range_binders`.
  (query-exec
   (create-view
    sub_range_defs
    (select
     d.from_path
     d.from_subs
     d.from_id
     (as (case [(is-not-null s.sub_ofs)  s.sub_ofs]  [else 0    ]) sub_ofs)
     (as (case [(is-not-null s.sub_span) s.sub_span] [else 65535]) sub_span)
     (as (case [(is-not-null s.sub_beg)  s.sub_beg]  [else d.beg]) beg)
     (as (case [(is-not-null s.sub_end)  s.sub_end]  [else d.end]) end)
     #:from
     (left-join
      (as defs d)
      (as sub_range_binders s)
      #:on (and (= d.from_path s.path)
                (= d.from_subs s.subs)
                (= d.from_id   s.full_id))))))

  ;; This view joins `def_arrows` and `sub_range_binders`, thereby
  ;; producing multiple arrows from uses of identifiers with sub-range
  ;; binders.
  (query-exec
   (create-view
    sub_range_def_arrows
    (select
     d.use_path
     (as (case [(is-not-null s.sub_ofs)
                (+ d.use_beg s.sub_ofs)]
               [else d.use_beg])
         use_beg)
     (as (case [(and (is-not-null s.sub_ofs) (is-not-null s.sub_span))
                (+ d.use_beg s.sub_ofs s.sub_span)]
               [else d.use_end])
         use_end)
     (as (case [(is-not-null s.sub_id) s.sub_id] [else d.use_text]) use_text)
     d.use_stx
     d.kind
     d.def_beg
     d.def_end
     d.def_text
     d.def_stx
     d.from_path
     d.from_subs
     d.from_id
     (as (case [(is-not-null s.sub_ofs)  s.sub_ofs]  [else 0    ]) sub_ofs)
     (as (case [(is-not-null s.sub_span) s.sub_span] [else 65535]) sub_span)
     #:from
     (left-join
      (as def_arrows d)
      (as sub_range_binders s)
      #:on (and (<> d.kind 0) ;not lexical
                (= d.from_path s.path)
                (= d.from_subs s.subs)
                (= d.from_id   s.full_id))))))

  ;; This view abstracts over the difference between arrows for
  ;; lexical definitions and arrows for imported definitions. It left
  ;; joins `sub_range_def_arrows` on `sub_range_defs` table; note that
  ;; def_beg and def_end may be sql-null when the defining file has
  ;; not yet been analyzed.
  (query-exec
   (create-view
    def_xrefs
    (select
     a.use_path
     a.use_beg
     a.use_end
     a.use_text
     a.use_stx
     (as (case #:of kind [0 a.use_path] [else a.from_path]) def_path)
     (as (case #:of kind [0 a.def_beg]  [else d.beg])       def_beg)
     (as (case #:of kind [0 a.def_end]  [else d.end])       def_end)
     (as (case #:of kind [0 a.def_text] [else a.from_id])   def_text)
     #:from (left-join
             (as sub_range_def_arrows a)
             (as sub_range_defs d)
             #:using from_path from_subs from_id sub_ofs sub_span))))

  (query-exec
   (create-view
    def_xrefs_view
    (select
     (as (select str #:from strings #:where (= strings.id use_path)) use_path)
     use_beg
     use_end
     (as (select str #:from strings #:where (= strings.id use_text)) use_text)
     (as (select str #:from strings #:where (= strings.id use_stx))  use_stx)
     (as (select str #:from strings #:where (= strings.id def_path)) def_path)
     def_beg
     def_end
     (as (select str #:from strings #:where (= strings.id def_text)) def_text)
     #:from def_xrefs)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Name graph

  ;; Given some use, to find the set of same-named sites across 1 or
  ;; more files, we need traverse a different graph than the graph for
  ;; definitions.
  ;;
  ;; 0. A lexical arrow: The definition site is the final set member.
  ;;
  ;; 1. An import.
  ;;    - If it renames, the new name is the final set member.
  ;;    - Else continue following the graph with {nom-path
  ;;      nom-submods nom-id}.
  ;;
  ;; 2. An export:
  ;;    - If it renames, the new name is the final set member.
  ;;    - Else continue folloiwng the graph.

  ;; Like `def_arrows` but for name introductions as opposed to
  ;; definitions. Using a separate table lets us handle things
  ;; differently (such as arrows for prefix-in prefixes) as well as
  ;; recording additional arrows that aren't necessary for defintions.
  (create-table/interned
   name_arrows
   #:columns
   [use_path    string]
   [use_beg     integer]
   [use_end     integer]
   ;; `use_text` is the annotated text at the use site, i.e. it is
   ;; the [use_beg use_end) interval of use_path. It can be a
   ;; substring of the `use_stx` column, in the case of multiple
   ;; arrows for sub-sections of one identifier, arising from e.g.
   ;; prefix-in or the 'sub-range-binders syntax property.
   [use_text    string]
   [use_stx     string]
   ;; One of {0="lexical" 1="require" 2="module-lang"}
   [kind        integer]
   ;; When `kind` is 0 ("lexical"), this is the local definition
   ;; site. Otherwise, this is the require site.
   [def_beg     integer]
   [def_end     integer]
   [def_text    string] ;text at def site
   [def_stx     string] ;is this ever useful??
   ;; Unless `kind` is 0 ("lexical"), these correspond to
   ;; identifier-binding nominal-from-xxx items. Specifically join
   ;; these on the `exports` table to find the location, within the
   ;; file, if already known. When kind="lexical", only nom_path is
   ;; meaningful and is simply the same as use_path.
   [nom_path    string] ;nominal-from-mod
   [nom_subs    string] ;nominal-from-mod
   [nom_id      string]
   #:constraints
   (primary-key use_path use_beg use_end)
   (check       (in kind #:values 0 1 2))
   ;; We use negative positions for anonymous all-from-out provides,
   ;; so we DON'T check for positive positions here.
   (check       (< use_beg use_end))  ;half-open interval
   (check       (< def_beg def_end))) ;half-open interval)

  (create-table/interned
   exports
   #:columns
   [nom_path    string]
   [nom_subs    string]
   [nom_id      string]
   [beg         integer]
   [end         integer]
   #:constraints
   (primary-key nom_path nom_subs nom_id)
   ;; We use negative positions for anonymous all-from-out provides,
   ;; so we DON'T check for positive positions here.
   (check       (< beg end))) ;half-open interval

  ;; This view joins `exports` and `sub_range_binders`.
  (query-exec
   (create-view
    sub_range_exports
    (select
     e.nom_path
     e.nom_subs
     e.nom_id
     (as (case [(is-not-null s.sub_ofs)  s.sub_ofs]  [else 0    ]) sub_ofs)
     (as (case [(is-not-null s.sub_span) s.sub_span] [else 65535]) sub_span)
     (as (case [(is-not-null s.sub_beg)  s.sub_beg]  [else e.beg]) beg)
     (as (case [(is-not-null s.sub_end)  s.sub_end]  [else e.end]) end)
     #:from
     (left-join
      (as exports e)
      (as sub_range_binders s)
      #:on (and (= e.nom_path s.path)
                (= e.nom_subs s.subs)
                (= e.nom_id   s.full_id))))))

  ;; This view joins `name_arrows` and `sub_range_binders`, thereby
  ;; producing multiple arrows from uses of identifiers with sub-range
  ;; binders.
  (query-exec
   (create-view
    sub_range_name_arrows
    (select
     a.use_path
     (as (case [(is-not-null s.sub_ofs)
                (+ a.use_beg s.sub_ofs)]
               [else a.use_beg])
         use_beg)
     (as (case [(and (is-not-null s.sub_ofs) (is-not-null s.sub_span))
                (+ a.use_beg s.sub_ofs s.sub_span)]
               [else a.use_end])
         use_end)
     (as (case [(is-not-null s.sub_id) s.sub_id] [else a.use_text]) use_text)
     a.use_stx
     a.kind
     a.def_beg
     a.def_end
     a.def_text
     a.def_stx
     a.nom_path
     a.nom_subs
     a.nom_id
     (as (case [(is-not-null s.sub_ofs)  s.sub_ofs]  [else 0    ]) sub_ofs)
     (as (case [(is-not-null s.sub_span) s.sub_span] [else 65535]) sub_span)
     #:from
     (left-join
      (as name_arrows a)
      (as sub_range_binders s)
      #:on (and (<> a.kind 0) ;not lexical
                (= a.nom_path s.path)
                (= a.nom_subs s.subs)
                (= a.nom_id   s.full_id))))))

  ;; Like def_xrefs, but uses name_arrows and nom_{path subs id}.
  (query-exec
   (create-view
    name_xrefs
    (select
     a.use_path
     a.use_beg
     a.use_end
     a.use_text
     a.use_stx
     (as (case #:of kind [0 a.use_path] [else a.nom_path]) nom_path)
     (as (case #:of kind [0 a.def_text] [else a.nom_id])   nom_id)
     (as (case #:of kind [0 a.def_beg]  [else e.beg])      def_beg)
     (as (case #:of kind [0 a.def_end]  [else e.end])      def_end)
     #:from (left-join
             (as sub_range_name_arrows a)
             (as sub_range_exports e)
             #:using nom_path nom_subs nom_id sub_ofs sub_span))))

  (query-exec
   (create-view
    name_xrefs_view
    (select
     (as (select str #:from strings #:where (= strings.id use_path)) use_path)
     use_beg
     use_end
     (as (select str #:from strings #:where (= strings.id use_text)) use_text)
     (as (select str #:from strings #:where (= strings.id use_stx))  use_stx)
     (as (select str #:from strings #:where (= strings.id def_path)) def_path)
     def_beg
     def_end
     (as (select str #:from strings #:where (= strings.id def_text)) def_text)
     #:from name_xrefs))))
