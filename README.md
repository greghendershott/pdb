This is WIP exploring the idea of storing, for multiple source files,
the result of running drracket/check-syntax, plus some more analysis.

The main motivation is to support **multi-file** flavors of things
like "find references" and "rename".

The intent is this could enhance Racket Mode, as well as Dr Racket
and other tools.

# Database

For each analyzed source file:

1. Fully expand, accumulating some information even if expansion
   fails (as used by e.g. Typed Racket):

  - direct calls to `error-display-handler`
  - `online-check-syntax` logger messages

2. Run [check-syntax], recording the values from various
[`syncheck-annotations<%>`] methods.

After accumulating information in various fields of a struct, finally
the struct is serialized, compressed, and stored in a sqlite table.

[check-syntax]: https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html
[`syncheck-annotations<%>`]: https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html#%28def._%28%28lib._drracket%2Fcheck-syntax..rkt%29._syncheck-annotations~3c~25~3e%29%29

We extend the check-syntax analysis in various ways:

- In addition to `syncheck:add-definition-target`, which identifies
  definitions, we identify and record _exports_ from fully-expanded
  `#%provide` forms.
  
- In addition to `syncheck:add-arrow/name-dup/pxpy`, which identifies
  lexical and import arrows, we identify and record some other flavors
  of arrows:
  
  - import-rename-arrows, as from `rename-in` etc.
  - export-rename-arrows, as from `rename-out`, etc.

  Also we enhance the check-syntax import-arrows to store the "from"
  and "nominal-from" information from [identifier-binding]. Following
  the nominal-from values to the exports in other files, and vice
  versa, is how we can identify rename-sites across multiple files.

- We assemble a list of imported symbols, suitable for use as
  completion candidates, akin to `namespace-mapped-symbols`.
  [Currently this is one dumb flat list, as is done by Racket Mode's
  current back end. A to-do is to create a tree structure reflecting
  which candidates are valid where.]

[identifier-binding]:https://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29

## You want to jump where, in what size steps?

In Racket a definition can be exported and imported an arbitrary
number of times before it is used -- and can be renamed at each such
step.

In general, the definition graph elides that and expresses "big,
direct jumps" among files. Which is wonderful when you want to e.g.
"visit/find/jump to definition" in another file.

By contrast the "name introduction and use" graph cares about the
chain of exports and imports, and considers steps where a rename
occurs. A motivation is to support multi-file rename commands. For
that to work, every occurrence of the "same" name must be known,
including uses in `provide` and `require` forms, and considering
clauses like `rename-out`, `prefix-ix`, `rename-in`, `prefix-out`, and
so on.

For example, if user wants `foo` to be renamed `bar`, then sites like
`(provide foo)` must be changed. Furthermore, sites like `(provide
(rename-out [foo xxx]))` are inflection points where the graph ends.
If some other file does `(require (rename-in mod [xxx foo]))`, _that_
"foo" is not the same and should not be in the same set of sites to be
renamed as the "foo" in the exporting file.

## use->def vs. def->uses

For either type of graph, it is simple to proceed from a use to its
source. When the source is in some other file, we know _which_ other
file: The `identifier-binding` "from" or "nominal-from" information
always says in which other file to look. If that file isn't yet in the
database (or is outdated), we analyze it, and so on transitively.
Furthermore it is a 1:1 relation; even when there are multiple steps
(such as hopping through a contract wrapper to the wrapped
definition), each step is 1:1.

On the other hand, proceeding from a definition to its uses is a
1:many relation, transitively (each of the many uses may in turn have
many uses). Furthermore we can't discover absolutely all uses --
unless absolutely all using files have already been analyzed. There
exists only a set of _known_ uses, which is limited by the set of
already-analyzed files.

This is another motivation to save analysis results for multiple files
in a database. One or more directory trees, each for some project the
user cares about, can be analyzed proactively. (Thereafter a digest
mismatch can trigger an automatic re-analysis of a changed file.) This
enables discovering all uses, at least within the scope of those
projects.

# Disposition

## Racket Mode

Status quo, Racket Mode's back end runs check-syntax and returns to
the front end `racket-xp-mode` the full results for each file. The
entire Emacs buffer is re-propertized. For example mouse-overs become
`help-echo` text properties.

How exactly would Racket Mode's back end use this `pdb` project.

### Roadmap step 1: Still all results at once

Initially, Racket Mode's back end could use this pdb project the same
way: Get the full analysis results, and re-propertize the entire
buffer.

That alone is no improvement. But we could add new Racket Mode
commands that query the db, such as multi-file xref-find-references or
renaming.

Furthermore, I think we could eliminate the back end's cache of fully
expanded syntax. For example find-definition no longer needs to walk
fully-expanded syntax looking for a site. We already did that, for all
definitions, and saved the results; now it's just a db query.

(I'm not sure about find-signature: Maybe we could add a pass to walk
pre-expanded surface syntax, finding all signatures, as the status quo
back end does one by one.)

---

**Status**: Done as an initial sanity check, then discarded. I
modified `racket-xp-mode` and the Racket Mode back end to use pdb when
available, and use the same propertize-all-buffer approach. It
performed about the same as before; having multi-file rneame was nice.
Although that's still in the commit history, I wanted to move on past
that to the next step.

### Step 2: Query results JIT for spans

A bigger change: The front end would query just for various spans of
the buffer, as-needed.

This would improve how we handle larger files like
[class-internal.rkt], not to mention eenormous files like the [example
provided by samth].

[example provided by samth]: https://github.com/greghendershott/racket-mode/issues/522
[class-internal.rkt]: https://github.com/racket/racket/blob/master/racket/collects/racket/private/class-internal.rkt

Status quo, Emacs doesn't block while the analysis is underway, but
after it completes, for a sufficiently large buffer and analysis
results, it takes a very long time to marshal the results and to
re-propertize the entire buffer; Emacs can noticeably freeze.

Admittedly doing limited, JIT queries doesn't magically transform
drracket/check-syntax itself to a "streaming" or incremental approach.
The _entire_ analysis would still need to complete (still taking about
10 seconds for [class-internal.rkt], and 60 for the [example provided
by samth]!) before _any_ new results were available. However the
results could be retrieved in vastly smaller batches. IOW there would
still be a large delay until any new results were available, but no
update freezes.

---

**Status:** Done. Still dog-fooding. I quickly realized that modifying
`racket-xp-mode` to work in both the "classic" and new ways was going
to be messy. Instead I made a fresh `racket-pdb-mode`. This works by
doing a query to the db whenever point (Emacs jargon, a.k.a. the
caret) moves. The back end and pdb return values only pertaining to
point and the currently visible span (the window-start through
window-end positions, in Emacs jargon). I'm still dog-fooding this,
looking for problems or mis-features.

## Other tools

Of course this could become a package to be used in various other
ways.

We could offer any of:

- A CLI (e.g. a new `raco` tool).

- A stable API for Racket programs.

- An equivalent API via HTTP.

One issue here is that some tools might prefer or need line:column
coordinates instead of positions. [Effectively drracket/check-syntax
and our own analysis use `syntax-position` and `syntax-span`, ignoring
`syntax-line` and `syntax-column`.] Either we could try to store
line:col-denominated spans, also, in the db when we analyze (at some
cost in space). Or we could just synthesize these as/when needed by
such an API, by running through the file using `port-count-lines!` (at
some cost in time).

# Known limitations and to-do

- In `example.rkt`, `prefix-tests` has a couple tests I can't yet make
  pass, for the reasons explained in the comment: 1. `prefix-out`
  doesn't support sub-range-binders, and, 2. `all-defined-out` gives
  every definition the same srcloc, which is the `(all-defined-out)`
  form itself.

- The `#%provide` clauses `all-defined`, `all-defined-except`,
  `prefix-all-defined`, and `prefix-all-defined-except` are not yet
  supported by our analysis that finds exports. (Note that `provide`
  clauses like `all-defined-out` do not actually expand into these,
  and _are_ supported. So this limitation isn't as big as it seems.
  But if some handwritten code or other macro expansion uses these
  specific `#%provide` clauses, the exports won't be identified.)

- The `rename-sites` command currently returns a hash-table value with
  all results. For renames involving a huge number of files and sites,
  a for-each flavor might be preferable.

# Tire kicking

If you want to kick the tires on this in its current state, I
recommend looking at the tests in `example.rkt`, as called from the
`tests` submodule.

As the functions work in terms of 1-based positions, just like Racket
`syntax-position` and Emacs buffer positions, it's annoying to keep
typing <kbd>C-x =</kbd> to see the position at point while in the
example files. You might find it handy to add something like the
following to your Emacs `mode-line-position` definition:

```elisp
(:propertize (:eval (format "%s" (point)))
             face (:slant italic))
```

Also remember that <kbd>M-g c</kbd> will let you jump to a position.

---

You probably want to avoid, however, the `very-many-files-example`
submodule -- unless you want to wait hours for very many files to be
analyzed:

```racket
  (require pdb)
  (for ([d (in-list (list* (get-pkgs-dir 'installation)
                           (get-pkgs-dir 'user)
                           (current-library-collection-paths)))])
    (when (directory-exists? d)
      (time (add-directory d #:import-depth 32767))))
  (require (submod pdb/store maintenance))
  (displayln (db-stats))
```

On my system -- with the non-minimal Racket distribution installed,
and about a dozen other packages:

```
--------------------------------------------------------------------------
Analysis data for 8124 source files: 183.5 MiB.

596394 nominal imports of 149866 exports: 3.2 MiB.
7667 interned paths: 0.6 MiB.

Total: 187.2 MiB.
Does not include space for integer key columns or indexes.

/home/greg/.racket/pdb/pdb-main.sqlite: 219.4 MiB.
Actual space on disk may be much larger due to deleted items: see VACUUM.
-------------------------------------------------------------------------
```

Also, if you use Emacs, you _could_ try the new `pdb` branch from the
`racket-mode` repo. In this case you probably to change your
`racket-mode-hook` to use `racket-pdb-mode` instead of
`racket-xp-mode`.
