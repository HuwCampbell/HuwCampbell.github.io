---
title: Maintaining Optparse-Applicative
description: Recent additions to optparse-applicative
tags: haskell
date: 2017-02-28
atUri: "at://did:plc:6e7msnu5o3mdboitzv7oxbxm/site.standard.document/3mdos5b2isf2m"
---

I'm the current maintainer of
[optparse-applicative](https://github.com/pcapriotti/optparse-applicative),
and have been since about about August 2015. It was already a
fantastic library when I took over looking after it, and Paolo
Capriotti really deserves all the credit for the nature of the
library. It's been an absolute honour to look after optparse, and
I'm very pleased to have been trusted to do so.

In this post, I'll talk about some of the changes I have made, and
my approach to my role as maintainer.

My first priority with optparse-applicative is to keep it simple,
not be too eager to merge things, and break as little as possible.
As optparse-applicative is used for the creation of applications,
it's very hard to know its penetration and number of users, but it's
quite significant. My second priority is that it remains an exact
library, with well defined semantics, which encourage the creation
of good CLI interfaces. Finally, I'd like to keep it being the best
command line parsing library, not only in Haskell, but for any
language out there.


Error Messages
--------------

One of my first patches, with 0.12, was good error messages for
when the user misses a mandatory term. There used to be particularly
nasty situations one could get themselves into when subcommands were
involved, where the parse would fail with no error message, and the
item they were missing wasn't even mentioned in the usage line they
were shown.

Now when something is missing, we have an error message like
```
Missing: (-a | -b)
```
for example.

With 0.13, the implementation of this feature became much nicer.
Now, when the parser fails, the error data type is filled with the
partially completed, existentially wrapped parser.

```haskell
data SomeParser = forall a . SomeParser (Parser a)

data ParseError
  = MissingError ... SomeParser
  | ...
```

This `SomeParser` can be traversed, and the usage line printed for
it printed (including only options with no default value).

In addition to a few extra error message clean ups and additions,
I believe that there are now no cases where the parser can fail and
one won't receive a useful error message. From a usability
standpoint this is a big deal.

Unix style grouping of short options
------------------------------------

Most command line tools allow short options to be grouped together,
```
tar -xcf
```
for instance is a valid command. This is pretty much a posix
requirement, and it was a bit embarrassing that optparse didn't support
it.

Fixing this was my first patch optparse. It works by popping off the
rest of the flags from the command if the first is a flag and not
an option. So when considering `-xcf`, if it notices that `-x` is
a flag, it will parse it, then on the next step of the parse, examine
`-cf`.

Suggestions
-----------

This one hasn't landed in `HEAD` yet, but was kind of fun. When I
realised that I can use the same machinery we use for bash completion
and the *missing:* error message generation it became quite easy to
implement cleanly.  When an option, flag, or subcommand can't be
parsed, or more specifically no match for it could be found, we
include not only the unknown term, but also the existentially wrapped
parser in the intermediate error data type.

```haskell
data ParseError
  = MissingError ... SomeParser
  | UnexpectedError String SomeParser
  | ...
```

Then, we examine the `SomeParser`, but instead of including only
mandatory options, we include only reachable options. A list of
potential terms is generated, and they filtered based on the
[Damerau–Levenshtein](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
distance to the unrecognised option the user provided.

The end result looks a lot like git's command completion, and is quite
nice I think
```
Invalid argument `installl'

Did you mean this?
    install
```
```
Invalid option `-x'

Did you mean one of these?
    -v
    -h
```

I might do a bit more around short options, at the moment the whole
term is considered, but it might be good to also compare just the
short term too.

Command Groups
--------------

This addition allows one to separate out logical groups of subcommands
based on some semantic reason. Some potential uses would be to
separate common from unusual examples for instance; or safe from
dangerous.

This sort of idea has been available with other libraries for a
while, so it seemed like time to add it to optparse-applicative as
well.

The idea is simple enough, one can specify that this set of subcommands
belongs to a particular group. There was however a bit of extra
work to ensure that the usage text will remain exactly the same if
the user doesn't provide these terms, no matter how they defined
their parser.

Here's a quick example:
```
Usage: commands COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  hello                    Print greeting
  goodbye                  Say goodbye

French commands:
  bonjour                  Print greeting
  au-revoir                Say goodbye
```

Conclusion
----------

These are all pretty minor changes which don't really change the way
that a programmer interacts with the library at all. They just deliver
a nicer set of error messages, usability improvements and better and
usage help text.
