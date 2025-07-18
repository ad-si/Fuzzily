{-|
Fuzzy string search in Haskell.
Uses 'TextualMonoid' to be able to run on different types of strings.
-}
module Text.Fuzzily where

import Protolude (
  Bool (True),
  Char,
  Down (Down),
  Eq ((==)),
  Int,
  Maybe (..),
  Monoid (mempty),
  Num ((*), (+)),
  Ord ((>)),
  Semigroup ((<>)),
  Show,
  const,
  identity,
  isJust,
  map,
  mapMaybe,
  not,
  otherwise,
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
Run one-pass algorithm on the given search text.
Returns (rendered, score) if the whole pattern was consumed.
-}
matchOnce
  :: (T.TextualMonoid text)
  => (Char -> Char)
  -- ^ normalisation function
  -> (text, text)
  -- ^ (pre, post)
  -> text
  -- ^ pattern
  -> text
  -- ^ search text
  -> Maybe (text, Int)
  -- ^ (rendered, score)
matchOnce norm (pre, post) pat txt = do
  let
    (tot, _, res, restPat) =
      T.foldl_'
        ( \(tot_, cur, acc, p) c -> case T.splitCharacterPrefix p of
            Nothing -> (tot_, 0, acc <> T.singleton c, p)
            Just (x, xs)
              | norm x == norm c ->
                  let cur' = cur * 2 + 1
                  in  ( tot_ + cur'
                      , cur'
                      , acc <> pre <> T.singleton c <> post
                      , xs
                      )
              | otherwise -> (tot_, 0, acc <> T.singleton c, p)
        )
        (0, 0, mempty, pat)
        txt

  if null restPat then Just (res, tot) else Nothing


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
{-# INLINEABLE match #-}
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
match caseSen preAndPost extract pat value = do
  let
    norm = if caseSen == HandleCase then identity else toLower
    searchText = extract value

    -- iterate over every suffix while carrying the already-passed prefix
    go pref txt best =
      case matchOnce norm preAndPost pat txt of
        Just (rendSub, sc) ->
          let cand = Fuzzy value (pref <> rendSub) sc
              best' = chooseBetter cand best
          in  step best'
        Nothing -> step best
      where
        step b = case T.splitCharacterPrefix txt of
          Nothing -> b
          Just (c, rest') -> go (pref <> T.singleton c) rest' b

    chooseBetter n Nothing = Just n
    chooseBetter n (Just o) = if score n > score o then Just n else Just o

  go mempty searchText Nothing


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
{-# INLINEABLE filter #-}
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
filter caseSen (pre, post) extractFunc textPattern texts =
  sortOn
    (Down . score)
    ( mapMaybe
        (match caseSen (pre, post) extractFunc textPattern)
        texts
    )


{-|
Return all elements of the list that have a fuzzy
match against the pattern. Runs with default settings where
nothing is added around the matches, as case insensitive.

>>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
["vim","virtual machine"]
-}
{-# INLINEABLE simpleFilter #-}
simpleFilter
  :: (T.TextualMonoid text)
  => text
  -- ^ Pattern to look for.
  -> [text]
  -- ^ List of texts to check.
  -> [text]
  -- ^ The ones that match.
simpleFilter textPattern xs =
  map
    original
    (filter IgnoreCase (mempty, mempty) identity textPattern xs)


{-|
Returns false if the pattern and the text do not match at all.
Returns true otherwise.

>>> test "brd" "bread"
True
-}
test :: (T.TextualMonoid text) => text -> text -> Bool
test textPattern text =
  isJust (match IgnoreCase (mempty, mempty) identity textPattern text)
