# Bad Datalog

This is a not-so-good datalog that someday might grow into a less-bad datalog.

It's got some stuff in it:

- A simple relational database for storage and queries
- Datalog frontend to give a nicer frontend for writing queries
- All in Elm, so you can embed it in your frontend project (if you're using Elm... which I am!)

It still needs some stuff before it's "good":

- A sample app
- Aggregation of any kind
- Any level of query plan optimization (in fact right now we definitely have the worst possible performance in a lot of cases.)
- It might not be safe to store databases in the model (that is, it will break the Elm debugger because the Set implementation we use embeds a comparison function. It would be possible to take this out, but would require more effort than I want to make at the moment I'm writing this README.)
- Nice errors that say exactly where the problem is in the query
- A DSL to parse from a string to a datalog program (this would make it way easier to introduce new variables, among other things.)
- Warnings for situations where queries might not work the way you expect (e.g. doing `reachable(a, c) :- reachable(a, b), reachable(b, c).` instead of `reachable(a, c) :- link(a, b), reachable(b, c).`)
- Indexes or primary keys of any kind for faster queries
- Named fields instead of just using positional semantics.

I plan to implement those things in roughly that order (maybe adding or dropping a few as I go) and releasing this as an Elm package when it's ready.
The rankings are in `next-features.json` in the root of this repo, and can be fed into [elo.bytes.zone](https://elo.bytes.zone) for further ranking.

## History

A prior version of this project ([88454c3c](https://git.bytes.zone/brian/bad-datalog/commit/88454c3cf2153121384735fe5488286e724eef54) and prior) used prolog semantics internally instead of a relational algebra (think "lots of loops" instead of "query plan".)
This turned out to be kind of a hassle, unfortunately, and I felt very stuck.

Fortunately, a kind someone from the [Recurse Center](https://www.recurse.com/) helped me a lot by pointing out that industrial datalogs have gone away from prolog semantics to relational algebras, and that I should consider doing the same.
That got me unstuck, and now here we are!

If you used [datalog.bytes.zone](https://datalog.bytes.zone) before, you'll have used the prolog-like version.
I haven't gotten to the point where it makes sense to write the parser for the DSL for the new version, but soon the current code in this repo will live at that location too!

Here are some miscellaneous ideas on the Elm Discourse which introduce what I think might be possible: [Brainstorming: relational programming in Elm](https://discourse.elm-lang.org/t/brainstorming-relational-programming-in-elm/6695/7)

## License

This doesn't have a license yet.
It'll probably be MIT or BSD-3-Clause or something similar eventually, but for now it's source-available but all-rights-reserved.
