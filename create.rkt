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
    [from_path   integer] ;from-mod
    [from_subs   integer] ;from-mod
    [from_id     integer] ;from-sym
    #:constraints
    (primary-key use_path use_beg use_end)
    (check (in kind #:values 0 1 2))
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
    ;; key is -- the triple (path subs sym).
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [sym         integer #:not-null]
    ;; Otherwise we just record the [beg end) location within the
    ;; file.
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key path subs sym)
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key sym #:references (strings id))))

  ;; This view abstracts over the difference between arrows for
  ;; lexical definitions and arrows for imported definitions. It left
  ;; joins `def_arrows` on the `defs` table for imports; note that
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
             (as def_arrows a)
             (as defs d)
             #:on
             (and (= a.from_path d.path)
                  (= a.from_subs d.subs)
                  (= a.from_id   d.sym))))))

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
    (foreign-key path #:references (strings id))
    (unique      path beg end)))

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
    [nom_id      integer] ;nominal-sym
    #:constraints
    (primary-key use_path use_beg use_end)
    (check (in kind #:values 0 1 2))
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
    [path        integer #:not-null]
    [subs        integer #:not-null]
    [sym         integer #:not-null]
    [beg         integer #:not-null]
    [end         integer #:not-null]
    #:constraints
    (primary-key path subs sym)
    (foreign-key path #:references (strings id))
    (foreign-key subs #:references (strings id))
    (foreign-key sym #:references (strings id))))

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
             (as name_arrows a)
             (as exports e)
             #:on
             (and (= a.nom_path e.path)
                  (= a.nom_subs e.subs)
                  (= a.nom_id   e.sym))))))

  ;;; Optional convenience views
  ;;;
  ;;; These aren't necessarily efficient; not intended for "real" use.
  ;;; However they have some documentation value, and, they can be
  ;;; handy when debugging, to explore seeing "de-interned", stringy
  ;;; values.

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
     (as (select str #:from strings #:where (= strings.id path)) path)
     (as (select str #:from strings #:where (= strings.id subs)) subs)
     (as (select str #:from strings #:where (= strings.id sym))  sym)
     beg
     end
     #:from defs)))

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
