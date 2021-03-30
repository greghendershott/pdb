This is WIP exploring the idea of populating a sqlite database of
defintions and uses discovered from running drracket/check-syntax.

The main motivation is to support **multi-file** flavors of things
like "find references" and "rename".

The intent is this could enhance Racket Mode, as well as the Dr Racket
IDE and other tools.

How exactly would Racket Mode's back end use this? Still TBD. Two
ideas:

1. One idea would be to *replace* the Racket Mode back end
check-syntax code with this: The front end would request an analysis,
the back end would notify when it's ready, and the front end would
issue commands to discover info for various intervals of the buffer.
Unclear if it would still propertize text ranges with some/all of that
returned info, or instead, if it could just query the back end as
needed. Although this could probably work, it would be slower. Our
analysis here takes ~ 1.5X to 2X the time, due to db writing/reading
overhead. Also it would mean storing much more in the db, for example
mouse-over messages, than the current WIP example does. [OTOH it might
be a way to contribute to fixing the "streaming for very large source
files" issue samth reported.]

2. Another idea is that the RMBE would still do and return its
analysis, status quo. It's just that, *in addition*, it would submit
some of these results to the db --- presumably using another thread
that doesn't delay the command response to the front end. That way,
the Emacs front end check-syntax would not change in method or timing.
However, we could support Racket Mode commands that use inter-file
references. Also, we could eliminate the back end's syntax caching, I
think: Things like find-definition need not walk fully-expanded syntax
to look for the definition site -- we already did that and saved the
result for all possible definitions, beforehand. So now that is just a
db query. (I'm not sure about find-signature: maybe we could add a
pass to walk non-expanded syntax, and store that extra info in a new
column in the `defs` table, or, store it in a new `sigs` table.)
