This is WIP exploring the idea of populating a sqlite database of
definitions and uses discovered from running drracket/check-syntax.

The main motivation is to support **multi-file** flavors of things
like "find references" and "rename".

The intent is this could enhance Racket Mode, as well as the Dr Racket
IDE and other tools.


# Database

As a first approximation, this runs [check-syntax] and stores the values
from various [`syncheck-annotations<%>`] methods in database tables.

[check-syntax]: https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html
[`syncheck-annotations<%>`]: https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html#%28def._%28%28lib._drracket%2Fcheck-syntax..rkt%29._syncheck-annotations~3c~25~3e%29%29

For example, `syncheck:add-unused-require` values go into an
`unused_requires` database table.

For some of the tables, there's not much more to the story. They are
equivalent to an on-disk `interval-map`. Given some path and position
within, you can query the database for annotations.

More interestingly, some of the tables effectively represent two
directed acyclic graphs: one for definitions, and the other for "name
introductions".

## Definition graph

Values from `syncheck:add-definition-target` go in a `defs` table.
Values from `syncheck:add-arrow/name-dup/pxpy` go in a `def_arrows`
table. When `require-arrow` is not false, then `def_arrows` may be
joined on `defs` to find the location of a definition in another file.
The `def_xrefs` view expresses such a join. The relation works both
ways: Given a use, you can find its definition. Given a definition,
you can find all its uses.

Note that the file containing the use might be analyzed before the
file containing the definition. In this case the join will produce SQL
NULL; if an answer is needed right away, we can detour to analyze the
defining file (we know the defining module path, just not the location
of the definition within), then retry.

## Name graph

Although check-syntax today does not have a "syncheck:add-export"
method, we've implemented one by doing our own, extra analysis (maybe
someday this could be merged into `drracket-tool-lib`). This
annotation corresponds to `#%provide` forms in fully-expanded syntax.
We add these to an `exports` table.

Values from `syncheck:add-arrow/name-dup/pxpy` go into a `name_arrows`
table. (Whereas `def_arrows` uses the `from-xxx` values from
[`identifier-binding`], `name_arrows` is oriented around the
`nominal-from-xxx` values.)

[`identifier-binding`]:https://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29

Similar to how the definition graph can join `def_arrows` on `defs`,
for a name graph we can join `name_arrows` on `exports`. The
`name_xrefs` view expresses such a join. The relation works both ways:
Given a use, you can find its name introduction site. Given a name
introduction site, you can find all its uses.

The name graph is interesting because it expresses the locations that
a multi-file rename command would need to change.

## You want to jump where?

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

(It's also possible we should store the fully-expanded syntax in the
database, too, as a cache that is available for any/all purposes. As
demonstrated by [`rfindler/fully-expanded-store`] a quoted syntax
object is serializable. If implemented/delivered, we could also build
on top of that cache instead of expanding ourselves.)

[rfindler/fully-expanded-store]:https://github.com/rfindler/fully-expanded-store

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

OTOH it might be a way to contribute toward a "streaming"
check-syntax, that could handle the extremely large source file
example provided by samth. It might mitigate the extreme worst cases
--- at some cost to the best case times.
