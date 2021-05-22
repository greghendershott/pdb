#lang racket/base

(require (prefix-in db: db)
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
  (query-exec
   (create-table
    #:if-not-exists imports
    #:columns
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [sym         integer #:not-null]
    #:constraints
    (primary-key path subs sym)
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key sym #:references (strings id))))

  ;; A table of syncheck:add-mouse-over-status annotations
  (query-exec
   (create-table
    #:if-not-exists mouseovers
    #:columns
    [path        integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    [text        integer #:not-null]
    #:constraints
    (check (< 0 beg))
    (check (< 0 end))
    (check (< beg end)) ;half-open interval
    (foreign-key path #:references (strings id))
    (foreign-key text #:references (strings id))
    (unique      path beg end text)))

  ;; A table of syncheck:add-tail-arrow annotations
  (query-exec
   (create-table
    #:if-not-exists tail_arrows
    #:columns
    [path        integer #:not-null]
    [tail        integer #:not-null]
    [head        integer #:not-null]
    #:constraints
    (check (< 0 tail))
    (check (< 0 head))
    (foreign-key path #:references (strings id))
    (unique      path tail head)))

  ;; A table of syncheck:add-unused-require annotations
  (query-exec
   (create-table
    #:if-not-exists unused_requires
    #:columns
    [path        integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (check (< 0 beg))
    (check (< 0 end))
    (check (< beg end)) ;half-open interval
    (foreign-key path #:references (strings id))
    (unique      path beg end)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Sub-range-binders. This is used by both the definition and name
  ;; graphs. Each row is similar to a single vector in a
  ;; sub-range-binders property value. So for instance there will be
  ;; rows for both <a-b a> and <a-b b>.
  (query-exec
   (create-table
    #:if-not-exists sub_range_binders
    #:columns
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [full_id     integer #:not-null]
    [sub_ofs     integer #:not-null]
    [sub_span    integer #:not-null]
    [sub_id      integer #:not-null]
    [sub_beg     integer #:not-null]
    [sub_end     integer #:not-null]
    #:constraints
    (primary-key path subs full_id sub_ofs sub_span)
    (check       (<= 0 sub_ofs))
    (check       (< 0 sub_span))
    (check       (< 0 sub_beg))
    (check       (< 0 sub_end))
    (check       (< sub_beg sub_end)) ;half-open interval
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key full_id #:references (strings id))
    (foreign-key sub_id #:references (strings id))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Definition graph

  ;; A table of arrows, both lexical and imported, as reported by
  ;; syncheck:add-arrow. In addition when the definition is imported
  ;; -- when `kind` is not "lexical" -- it also includes information
  ;; from identifier-binding -- a superset of what
  ;; syncheck:add-jump-to-definition supplies -- about the modpath
  ;; where it is defined. The precise location within that other file
  ;; can be found in `defs` after that file is also analyzed.
  (query-exec
   (create-table
    #:if-not-exists def_arrows
    #:columns
    [use_path    integer #:not-null]
    [use_beg     integer #:not-null]
    [use_end     integer #:not-null]
    ;; `use_text` is the annotated text at the use site, i.e. it is
    ;; the [use_beg use_end) interval of use_path. It can be a
    ;; substring of the `use_stx` column, in the case of multiple
    ;; arrows for sub-sections of one identifier, arising from e.g.
    ;; prefix-in or the 'sub-range-binders syntax property.
    [use_text    integer #:not-null]
    [use_stx     integer #:not-null]
    ;; One of {0="lexical" 1="require" 2="module-lang"}
    [kind        integer #:not-null]
    ;; When `kind` is 0 ("lexical"), this is the local definition
    ;; site. Otherwise, this is the require site.
    [def_beg     integer #:not-null]
    [def_end     integer #:not-null]
    [def_text    integer #:not-null] ;text at def site
    [def_stx     integer #:not-null] ;is this ever useful??
    ;; Unless kind is 0 ("lexical"), these correspond to
    ;; identifier-binding from-xxx items. Specifically, join these on
    ;; the `defs` table to find the location within the file, if
    ;; already known. When kind="lexical", only from_path is
    ;; meaningful and is simply the same as use_path.
    [from_path   integer] ;from-mod path
    [from_subs   integer] ;from-mod subs
    [from_id     integer] ;from-id
    #:constraints
    (primary-key use_path use_beg use_end)
    (check (in kind #:values 0 1 2))
    (check (< 0 use_beg))
    (check (< 0 use_end))
    (check (< use_beg use_end)) ;half-open interval
    (check (< 0 def_beg))
    (check (< 0 def_end))
    (check (< def_beg def_end)) ;half-open interval
    (foreign-key use_path #:references (strings id))
    (foreign-key use_text #:references (strings id))
    (foreign-key use_stx #:references (strings id))
    (foreign-key def_text #:references (strings id))
    (foreign-key def_stx #:references (strings id))
    (foreign-key from_path #:references (strings id))
    (foreign-key from_subs #:references (strings id))
    (foreign-key from_id #:references (strings id))))
  ;; TODO: Add indexes, e.g. for [def_beg def_end] columns?

  ;; A table of definitions in files, as reported by
  ;; syncheck:add-definition-target. Note that this does not include
  ;; sites of lexical definitions.
  (query-exec
   (create-table
    #:if-not-exists defs
    #:columns
    ;; Each definition is uniquely identified by -- i.e. the primary
    ;; key consists of -- these columns:
    [from_path   integer #:not-null]
    [from_subs   integer #:not-null]
    [from_id     integer #:not-null]
    ;; Otherwise we just record the [beg end) location within the
    ;; file.
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key from_path from_subs from_id)
    (check (< 0 beg))
    (check (< 0 end))
    (check (< beg end)) ;half-open interval
    (foreign-key from_path #:references (strings id))
    (foreign-key from_subs #:references (strings id))
    (foreign-key from_id #:references (strings id))))

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
  (query-exec
   (create-table
    #:if-not-exists name_arrows
    #:columns
    [use_path    integer #:not-null]
    [use_beg     integer #:not-null]
    [use_end     integer #:not-null]
    ;; `use_text` is the annotated text at the use site, i.e. it is
    ;; the [use_beg use_end) interval of use_path. It can be a
    ;; substring of the `use_stx` column, in the case of multiple
    ;; arrows for sub-sections of one identifier, arising from e.g.
    ;; prefix-in or the 'sub-range-binders syntax property.
    [use_text    integer #:not-null]
    [use_stx     integer #:not-null]
    ;; One of {0="lexical" 1="require" 2="module-lang"}
    [kind        integer #:not-null]
    ;; When `kind` is 0 ("lexical"), this is the local definition
    ;; site. Otherwise, this is the require site.
    [def_beg     integer #:not-null]
    [def_end     integer #:not-null]
    [def_text    integer #:not-null] ;text at def site
    [def_stx     integer #:not-null] ;is this ever useful??
    ;; Unless `kind` is 0 ("lexical"), these correspond to
    ;; identifier-binding nominal-from-xxx items. Specifically join
    ;; these on the `exports` table to find the location, within the
    ;; file, if already known. When kind="lexical", only nom_path is
    ;; meaningful and is simply the same as use_path.
    [nom_path    integer] ;nominal-from-mod
    [nom_subs    integer] ;nominal-from-mod
    [nom_id      integer]
    #:constraints
    (primary-key use_path use_beg use_end)
    (check (in kind #:values 0 1 2))
    ;; We use negative positions for anonymous all-from-out provides,
    ;; so we DON'T check for positive positions here.
    (check (< use_beg use_end)) ;half-open interval
    (check (< def_beg def_end)) ;half-open interval
    (foreign-key use_path #:references (strings id))
    (foreign-key use_text #:references (strings id))
    (foreign-key use_stx #:references (strings id))
    (foreign-key def_text #:references (strings id))
    (foreign-key def_stx #:references (strings id))
    (foreign-key nom_path #:references (strings id))
    (foreign-key nom_subs #:references (strings id))
    (foreign-key nom_id #:references (strings id))))

  (query-exec
   (create-table
    #:if-not-exists exports
    #:columns
    [nom_path    integer #:not-null]
    [nom_subs    integer #:not-null]
    [nom_id      integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key nom_path nom_subs nom_id)
    ;; We use negative positions for anonymous all-from-out provides,
    ;; so we DON'T check for positive positions here.
    (check (< beg end)) ;half-open interval
    (foreign-key nom_path #:references (strings id))
    (foreign-key nom_subs #:references (strings id))
    (foreign-key nom_id #:references (strings id))))

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

  ;;; Optional convenience views
  ;;;
  ;;; These aren't necessarily efficient; not intended for "real" use.
  ;;; However they have some documentation value, and, they can be
  ;;; handy when debugging, to explore seeing "de-interned", stringy
  ;;; values.

  (query-exec
   (create-view
    SubRangeBindersView
    (select
     (as (select str #:from strings #:where (= id path)) path)
     (as (select str #:from strings #:where (= id subs)) subs)
     (as (select str #:from strings #:where (= id full_id)) full_id)
     sub_ofs
     sub_span
     (as (select str #:from strings #:where (= id sub_id)) sub_id)
     sub_beg
     sub_end
     #:from sub_range_binders)))

  (query-exec
   (create-view
    DefArrowsView
    (select
     (as (select str #:from strings #:where (= strings.id use_path)) use_path)
     use_beg
     use_end
     (as (select str #:from strings #:where (= strings.id use_text)) use_text)
     (as (select str #:from strings #:where (= strings.id use_stx))  use_stx)
     (as (case #:of kind [0 "lexical"] [1 "require"] [2 "module-lang"] [else "!!!"]) kind)
     def_beg
     def_end
     (as (select str #:from strings #:where (= strings.id def_text))  def_text)
     (as (select str #:from strings #:where (= strings.id def_stx))   def_stx)
     (as (select str #:from strings #:where (= strings.id from_path)) from_path)
     (as (select str #:from strings #:where (= strings.id from_subs)) from_subs)
     (as (select str #:from strings #:where (= strings.id from_id))   from_id)
     #:from def_arrows)))

  (query-exec
   (create-view
    DefsView
    (select
     (as (select str #:from strings #:where (= strings.id from_path)) from_path)
     (as (select str #:from strings #:where (= strings.id from_subs)) from_subs)
     (as (select str #:from strings #:where (= strings.id from_id))   from_id)
     beg
     end
     #:from defs)))

  (query-exec
   (create-view
    DefXrefsView
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

  (query-exec
   (create-view
    NameArrowsView
    (select
     (as (select str #:from strings #:where (= strings.id use_path)) use_path)
     use_beg
     use_end
     (as (select str #:from strings #:where (= strings.id use_text)) use_text)
     (as (select str #:from strings #:where (= strings.id use_stx))  use_stx)
     (as (case #:of kind [0 "lexical"] [1 "require"] [2 "module-lang"] [else "!!!"]) kind)
     def_beg
     def_end
     (as (select str #:from strings #:where (= strings.id def_text))  def_text)
     (as (select str #:from strings #:where (= strings.id def_stx))   def_stx)
     (as (select str #:from strings #:where (= strings.id nom_path))  nom_path)
     (as (select str #:from strings #:where (= strings.id nom_subs))  nom_subs)
     (as (select str #:from strings #:where (= strings.id nom_id))    nom_id)
     #:from name_arrows)))

  (query-exec
   (create-view
    ExportsView
    (select
     (as (select str #:from strings #:where (= strings.id path)) path)
     (as (select str #:from strings #:where (= strings.id subs)) subs)
     (as (select str #:from strings #:where (= strings.id sym))  sym)
     beg
     end
     #:from exports))))
