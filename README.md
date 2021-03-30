This is WIP exploring the idea of populating a sqlite database of
defintions and uses discovered from running drracket/check-syntax.

The main motivation is to support **multi-file** flavors of things
like "find references" and "rename".

The intent is this could enhance Racket Mode, as well as the Dr Racket
IDE and other tools.

How exactly would Racket Mode's back end use this? Still TBD. Two
ideas so far:

# Enhance

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

# Replace

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
