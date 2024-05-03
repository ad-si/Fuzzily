# Fuzzily

Fuzzy string search library in Haskell.
Uses `TextualMonoid` from
[monoid-subclasses](https://hackage.haskell.org/package/monoid-subclasses)
to be able to run on different types of strings.

This is a fork of Joomy Korkut's [fuzzy](https://github.com/joom/fuzzy),
which itselft was a port of the JavaScript library
[mattyork/fuzzy](https://github.com/mattyork/fuzzy).

It was initially forked to be used in [TaskLite](https://tasklite.org/)'s
`find` sub-command.


## Usage

```haskell
> import Text.Fuzzily

> match "fnt" "infinite" ("", "") id HandleCase
Just (Fuzzily
        { original = "infinite"
        , rendered = "infinite"
        , score = 3
        })

> match "hsk" ("Haskell",1995) ("<", ">") fst IgnoreCase
Just (Fuzzily
        { original = ("Haskell", 1995)
        , rendered = "<h>a<s><k>ell"
        , score = 5
        })

> langs = [("Standard ML", 1990), ("OCaml", 1996), ("Scala", 2003)]
> filter "ML" langs ("<", ">") fst IgnoreCase
[ Fuzzily
    { original = ("Standard ML", 1990)
    , rendered = "standard <m><l>"
    , score = 4
    }
, Fuzzily
    { original = ("OCaml", 1996)
    , rendered = "oca<m><l>"
    , score = 4
    }
]

> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
["vim","virtual machine"]

> test "brd" "bread"
True
```
