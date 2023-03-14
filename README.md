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

In Racket a definition can be exported an imported an arbitrary number
of times before it is used -- and can be renamed at each such step.

In general, the **definition** graph "elides" that and expresses
"bigger jumps" among files. Which is wonderful when you want to e.g.
"jump to definition" in another file.

By contrast the **name** graph cares about the chain of exports and
imports, and especially steps where a rename occurs. A motivation is
to support multi-file rename commands. For that to work, every
occurrence of the name must be known. Including its use in `provide`
and `require` forms. For example, if `foo` is to be renamed `bar`,
then instances like `(provide foo)` must be changed, too. Furthermore,
rename points such as `(provide (rename-out [foo xxx]))` are
inflection points where the graph ends.

## use->def vs. def->uses

Either way, it is simple to proceed from a use to its definition. When
the definition is in some other file, we know _which_ other file. If
it's not yet in the database, we analyze it also, and so on
transitively.

On the other hand, proceeding from a definition to its uses has no
such lazy JIT method. The set of known uses is limited by the set of
already-analyzed files.

This is another motivation to save analysis results for multiiple
files in a database. A directory tree -- for a package or a project --
can be analyzed proactively, and the results reused. (Only a digest
mismatch need cause re-analysis of a changed file.)

# Disposition

## Racket Mode

Status quo, Racket Mode's back end runs check-syntax and returns to
the front end `racket-xp-mode` the full results for each file. The
entire buffer is re-propertized. For example mouse-overs become
`help-echo` text properties.

How exactly would Racket Mode's back end use this? Probably a
mini-roadmap with two steps.

### Step 1: Still all results at once

Initially, Racket Mode's back end could use this pdb project the same
way: Get the full analysis results, and re-propertize the entire
buffer.

That in itself is no improvement. But we could add new Racket Mode
commands that query the db, such as multi-file xref-find-references or
renaming.

Furthermore, I think we could eliminate the back end's cache of fully
expanded syntax. For example find-definition no longer needs to walk
fully-expanded syntax looking for a site. We already did that, for all
definitions, and saved the results; now it's just a db query.

(I'm not sure about find-signature: maybe we could add a pass to walk
non-expanded syntax for signatures.)

### Step 2: Query results JIT for spans

A bigger change: The front end would query just for various spans of
the buffer, as-needed. Using the same jit-font-lock strategy as in my
other WIP project, a "racket-hash-lang-mode" branch for Racket Mode.

This would probably improve how we handle extremely large source
files, as in the [example provided by
samth](https://github.com/greghendershott/racket-mode/issues/522).
Status quo, although Emacs doesn't block while the analysis is
underway, when finished re-propertizing a sufficiently large buffer
can cause a noticeable freeze.

Admittedly this wouldn't magically transform drracket/check-syntax
itself to a "streaming" approach. The entire analysis would still need
to complete, before any results were available. However the results
could be retrived in smaller batches. IOW there would still be a large
delay until any new results were availavle, but no update freezes.

## Other tools

Of course this could become a package to be used in various other
ways.

We could offer any of:

- A CLI (e.g. a new `raco` tool).

- A stable API for Racket programs.

- An equivalent API via HTTP.

One issue here is that some tools might prefer or need line:col
instead of positions. [Effectively drracket/check-syntax and our own
analysis use `syntax-postiion` and `syntax-span`, ignoring
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
`syntax-position` and Emacs buffer positions, you might it annoying to
keep typing <kbd>C-x =</kbd> to see the position at point while in the
example files. You might find it handy to add something like the
following to your Emacs `mode-line-position` definition:

```elisp
(:propertize (:eval (format "%s" (point)))
             face (:slant italic))
```

Also remember that <kbd>M-g c</kbd> will let you jump to a position.

---

You probably want to avoid, however, the `very-many-files-example`
submodule -- unless you want to wait hours for 8,000 files to be
analyzed for the first time:

```racket
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
```

