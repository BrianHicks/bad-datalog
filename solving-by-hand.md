Say I had the following program computing which parts of a graph were reachable from `a`:

```
link(a, b)
link(b, c)

reachable(From, To) :- link(From, To)
reachable(From, To) :- link(From, Middle), reachable(Middle, To)

query(To) :- reachable(a, To)
```

How would I solve this?

I guess I'd look at the rules as a first pass. Anything that doesn't have a body I'd just hold on to.

Then I guess I'd look at the first definition of `reachable`...

```
reachable(From, To) :- link(From, Middle)
```

I'd see what `link` rules I had. All of these will unify, so I guess I'd have a bunch of substitutions that looked like this:

```
From: a, To: b
From: b, To: c
```

I'd apply my substitutions to `reachable` to get two more rules:

```
reachable(a, b)
reachable(b, c)
```

My old implementation just kind of hangs onto those things until the next iteration. I'm not sure why they can't enter the IDB immediately.

---

Moving on to the second definition of `reachable`:

```
reachable(From, To) :- link(From, Middle), reachable(Middle, To)
```

First I'm looking at `link` again. I already found that this gives me the following substitutions:

```
From: a, Middle: b
From: b, Middle: c
```

Now for the recursive call to `reachable`. Since I already have substitutions, I can make the following partial replacements for the atom:

```
reachable(b, To)
reachable(c, To)
```

Now for each of those, I'll try to unify with all the facts I know about `reachable`. To start with, I don't know anything so this unification fails (which is not the implementation I may have arrived at by default, so I'm glad I'm thinking this through.)

Assuming the opposite (e.g. because I'm in the next iteration or whatever) I can unify with each fact. For the first partial derivation above, that looks like:

```
reachable(b, To) + reachable(a, b) -> doesn't unify
reachable(b, To) + reachable(b, c) -> From: a, Middle: b, To: c
```

For the other, it looks like:

```
reachable(c, To) + reachable(a, b) -> doesn't unify
reachable(c, To) + reachable(b, c) -> doesn't unify
```

So I get one sucessful unification. That's all the rules here, so I can do substitution on the head to get:

```
reachable(a, c)
```

---

Now on to `query`...

```
query(To) :- reachable(a, To)
```

Assuming that I have all the previously-derived facts, I've just got to look up substituions for `reachable` and then apply them to `query`. So I get:

```
query(b)
query(c)
```

Which is exactly what I want!
