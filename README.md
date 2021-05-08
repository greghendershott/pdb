This is WIP exploring the idea of populating a sqlite database of
defintions and uses discovered from running drracket/check-syntax.

The main motivation is to support **multi-file** flavors of things
like "find references" and "rename".

The intent is this could enhance Racket Mode, as well as the Dr Racket
IDE and other tools.


# Database

As a first approximation, this runs check-syntax and stores the
values from various `syncheck` methods in database tables.

For example, `syncheck:add-unused-require` values go into an
`unused_requires` database table.

For some of the tables, there's not much more to the story. They are
equivalent to an on-disk `interval-map`.

Some of the tables effectively respresent two directed acyclic graphs:
one for definitions, and the other for "name introductions".

## Definition graph

Values from `syncheck:add-definition-target` go in a `defs` table.
Values from `syncheck:add-arrow` go in a `def_arrows` table. When
`require-arrow` is not false, then `def_arrows` may be joined on
`defs` to find the location of a definition in another file. The
`def_xrefs` view expresses such a join.

Note that the file containing the use might be analyzed before the
file containing the definition. In this case the join will produce SQL
NULL; if an answer is needed, we can analyze the defining file (we
know the defning module path, just not the location of the definition
within), then retry.

## Name graph

Check-syntax does not have a "syncheck:add-export" method. Imagine
that it did, and that such calls arose from analysis of `#%provide`
forms in fully-expanded syntax. In that case, we add exports to an
`exports` table. (Today we do our own analysis to complement
check-syntax; maybe someday this will be merged.)

Also imagine that `syncheck:add-arrow` supplied, not just the
`from-xxx` values from `identifier-binding`, but also the
`nominal-from-xxx` values. In that case, we add arrows to a
`name_arrows` table. (Today, we use the full syntax object value
supplied to `syncheck:add-arrow` to obtain this; probably that's fine
forever, no merge request needed.)

Similar to how the definition graph can join `def_arrows` on `defs`,
for a name graph we can join `name_arrows` on `exports`. The
`name_xrefs` view expresses such a join.

The name graph is interesting because it expresses the locations that
a multi-file rename command would need to change.

## Contrast

In general, the **definition** graph expresses "bigger jumps" among
files. A definition can be (re)provided an arbitary number of times
before it is used. The `from-xxx` values of `identifier-binding`
"elide" this and supply an arrow from the using file directly to the
defining file. Which is wonderful when you want to e.g. "jump to
definition" in another file.

However the **name** graph, to support e.g. rename commands, must
consider every occurrence of the name (e.g. `(provide foo)` must be
changed when `foo` is renamed `bar`). So we care about "smaller
jumps"; we definitely do not want to skip over exports. Furthermore,
Racket allows something to be renamed, on export and import, an
arbitary number of times. A command to "rename this thing" must be
limted to the subset of the chain that shares the same name.

# Disposition

How exactly would Racket Mode's back end use this? Still TBD. Two
ideas so far:

## Enhance

One idea is that the Racket Mode back end would still do its
check-syntax analysis, status quo. It's just that, *in addition*, it
would submit some of the analysis results --- the discovered
definitions and uses --- to the db. Presumably it would submit using
another thread that doesn't delay the command response to the front
end.

The Emacs front end racket-xp-mode would not change how it handles
text properties for annotations.

At the same time, we could support new Racket Mode commands that query
the db, such as multi-file xref-find-references or renaming.

Furthermore, I think we could eliminate the back end's cache of fully
expanded syntax. For example find-definition no longer needs to walk
fully-expanded syntax looking for a site. We already did that, for all
definitions, and saved the results; now it's just a db query.

(I'm not sure about find-signature: maybe we could add a pass to walk
non-expanded syntax, and store that extra info in a new column in the
`defs` table, or, store it in a new `sigs` table.)

## Replace

A bigger change would be to *replace* the Racket Mode back end
check-syntax code with this. The front end would request an analysis,
the back end would notify when it's ready, and the front end would
issue commands to query annotations for various intervals of the
buffer.

Although this could probably work, it would be slower. Our analysis
here takes ~ 1.5X to 2X the time, due to db writing/reading overhead.
Also things that could be accessed directly via Emacs text properties
would now need to be commands to the back end.

Also it would mean storing much more in the db (for example mouse-over
text and tail call info) than the current WIP example does.

OTOH it might be a way to contribute toward a "streaming"
check-syntax, that could handle the extremely large source file
example provided by samth. It might mitigate the extreme worst cases
--- at some cost to the best case times.
