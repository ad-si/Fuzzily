{-|
Fuzzy string search in Haskell.
Uses 'TextualMonoid' to be able to run on different types of strings.
-}
module Text.Fuzzily where

import Protolude as P (
  Bool (True),
  Down (Down),
  Eq ((==)),
  Int,
  Maybe (..),
  Monoid (mempty),
  Num ((*), (+)),
  Semigroup ((<>)),
  Show,
  const,
  identity,
  isJust,
  map,
  mapMaybe,
  not,
  sortOn,
  toLower,
  (.),
 )

import Data.Monoid.Textual qualified as T


{-|
Included in the return type of `match` and `filter`.
Contains the original value given, the rendered string
and the matching score.
-}
data Fuzzy val prettyText = Fuzzy
  { original :: val
  , rendered :: prettyText
  , score :: Int
  }
  deriving (Show, Eq)


data CaseSensitivity
  = IgnoreCase
  | HandleCase
  deriving (Show, Eq)


null :: (T.TextualMonoid s) => s -> Bool
null =
  not . T.any (const True)


{-|
Returns the rendered output and the
matching score for a pattern and a text.
Two examples are given below:

>>> match HandleCase ("", "") identity "fnt" "infinite"
Just (Fuzzy
  { original = "infinite"
  , rendered = "infinite"
  , score = 3
  })

>>> match IgnoreCase ("<", ">") fst "hsk" ("Haskell", 1995)
Just (Fuzzy
  { original = ("Haskell", 1995)
  , rendered = "<h>a<s><k>ell"
  , score = 5
  })
-}
{-# INLINABLE match #-}
match
  :: (T.TextualMonoid text)
  => CaseSensitivity
  -- ^ Handle or ignore case of search text
  -> (text, text)
  -- ^ Text to add before and after each match
  -> (value -> text)
  -- ^ Function to extract the text from the container
  -> text
  -- ^ Pattern
  -> value
  -- ^ Value containing the text to search in
  -> Maybe (Fuzzy value text)
  -- ^ Original value, rendered string, and score
match caseSensitivity (pre, post) extractFunc pattern value = do
  let
    normFunc = if caseSensitivity == HandleCase
      then identity
      else toLower

    searchText = extractFunc value

    (totalScore, _, result, patternFromFold) =
      T.foldl_'
        ( \(tot, cur, res, pat) c ->
            case T.splitCharacterPrefix pat of
              Nothing ->
                ( tot
                , 0
                , res <> T.singleton c
                , pat
                )
              Just (x, xs) ->
                if normFunc x == normFunc c
                  then
                    let cur' = cur * 2 + 1
                    in  ( tot + cur'
                        , cur'
                        , res <> pre <> T.singleton c <> post
                        , xs
                        )
                  else
                    ( tot
                    , 0
                    , res <> T.singleton c
                    , pat
                    )
        )
        (0, 0, mempty, pattern)
        searchText

  if null patternFromFold
    then Just (Fuzzy value result totalScore)
    else Nothing


{-|
The function to filter a list of values
by fuzzy search on the text extracted from them.

>>> langs = [("Standard ML", 1990), ("OCaml", 1996), ("Scala", 2003)]
>>> filter "ML" langs ("<", ">") fst IgnoreCase
[ Fuzzy
  { original = ("Standard ML", 1990)
  , rendered = "standard <m><l>"
  , score = 4}
, Fuzzy
  { original = ("OCaml", 1996)
  , rendered = "oca<m><l>"
  , score = 4
  }
]
-}
{-# INLINABLE filter #-}
filter
  :: (T.TextualMonoid text)
  => CaseSensitivity
  -- ^ Handle or ignore case of search text
  -> (text, text)
  -- ^ Text to add before and after each match
  -> (value -> text)
  -- ^ Function to extract the text from the container
  -> text
  -- ^ Pattern
  -> [value]
  -- ^ List of values containing the text to search in
  -> [Fuzzy value text]
  -- ^ List of results, sorted, highest score first
filter caseSen (pre, post) extractFunc pattern texts =
  sortOn
    (Down . score)
    ( mapMaybe
        (match caseSen (pre, post) extractFunc pattern)
        texts
    )


{-|
Return all elements of the list that have a fuzzy
match against the pattern. Runs with default settings where
nothing is added around the matches, as case insensitive.

>>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
["vim","virtual machine"]
-}
{-# INLINABLE simpleFilter #-}
simpleFilter
  :: (T.TextualMonoid text)
  => text
  -- ^ Pattern to look for.
  -> [text]
  -- ^ List of texts to check.
  -> [text]
  -- ^ The ones that match.
simpleFilter pattern xs =
  map
    original
    (filter IgnoreCase (mempty, mempty) identity pattern xs)


{-|
Returns false if the pattern and the text do not match at all.
Returns true otherwise.

>>> test "brd" "bread"
True
-}
test :: (T.TextualMonoid text) => text -> text -> Bool
test pattern text =
  isJust (match IgnoreCase (mempty, mempty) identity pattern text)
