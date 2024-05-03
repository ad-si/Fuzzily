# Fuzzily

Fuzzy string search library in Haskell.
Uses `TextualMonoid` from
[monoid-subclasses](https://hackage.haskell.org/package/monoid-subclasses)
to be able to run on different types of strings.

This is a fork of Joomy Korkut's [fuzzy](https://github.com/joom/fuzzy),
which itselft was a port of the JavaScript library
[mattyork/fuzzy](https://github.com/mattyork/fuzzy).

It's main difference is more readable code and a cleaner API:

- Descriptive variable names
- ADTs instead of booleans
- …

It was initially forked to be used in [TaskLite](https://tasklite.org/)'s
`find` sub-command.


## Usage

```haskell
> import Text.Fuzzily

> match HandleCase ("", "") id "fnt" "infinite"
Just (Fuzzy
        { original = "infinite"
        , rendered = "infinite"
        , score = 3
        })

> match IgnoreCase ("<", ">") fst "hsk" ("Haskell", 1995)
Just (Fuzzy
        { original = ("Haskell", 1995)
        , rendered = "<H>a<s><k>ell"
        , score = 5
        })

> langs = [("Standard ML", 1990), ("OCaml", 1996), ("Scala", 2003)]
> filter IgnoreCase ("<", ">") fst "ML" langs
[ Fuzzy
    { original = ("Standard ML", 1990)
    , rendered = "Standard <M><L>"
    , score = 4
    }
, Fuzzy
    { original = ("OCaml", 1996)
    , rendered = "OCa<m><l>"
    , score = 4
    }
]

> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
["vim", "virtual machine"]

> test "brd" "bread"
True
```
