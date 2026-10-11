{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Fuzzy string search in Haskell.
Uses 'TextualMonoid' to be able to run on different types of strings.
-}
module Text.Fuzzily where

import Protolude (
  Bool (False, True),
  Char,
  Down (Down),
  Eq ((/=), (==)),
  Int,
  Maybe (..),
  Monad ((>>=)),
  Monoid (mconcat, mempty),
  Num ((*), (+), (-)),
  Ord ((<), (>=)),
  Ordering (GT),
  Show,
  Text,
  chr,
  const,
  forM_,
  identity,
  isJust,
  map,
  mapMaybe,
  not,
  ord,
  otherwise,
  pure,
  sortOn,
  toLower,
  ($),
  (.),
 )

import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (newArray, runSTUArray)
import Data.Array.Unboxed (UArray, listArray)
import Data.Char (isAsciiUpper)
import Data.Monoid.Textual qualified as T
import Data.Text qualified as Text


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


-- | Like 'toLower', but faster for ASCII characters
lowerChar :: Char -> Char
lowerChar c
  | isAsciiUpper c = chr (ord c + 32)
  | c < '\x80' = c
  | otherwise = toLower c


{-|
Score of matching the given number of characters,
all consecutive (the maximum possible score).
-}
contiguousScore :: Int -> Int
contiguousScore len =
  go len 0 0
  where
    go :: Int -> Int -> Int -> Int
    go 0 !tot !_ = tot
    go k !tot !cur = let cur' = cur * 2 + 1 in go (k - 1) (tot + cur') cur'


-- | Whether the pattern is a (not necessarily contiguous) subsequence
isSubsequenceOf :: Text -> Text -> Bool
isSubsequenceOf pat txt =
  case Text.uncons pat of
    Nothing -> True
    Just (p, ps) -> case Text.uncons (Text.dropWhile (/= p) txt) of
      Nothing -> False
      Just (_, rest) -> isSubsequenceOf ps rest


{-|
Find the start of the greedy match with the highest score
(the earliest one on ties) and return its score
and the positions of the matched characters.
The pattern must be non-empty and a subsequence of the text.
Only starts at occurrences of the first pattern character are tried,
as starting anywhere else yields the same result as starting
at the next occurrence.
Once a start fails, all later ones fail too.
-}
bestGreedyMatch :: Text -> Text -> Maybe (Int, [Int])
bestGreedyMatch pat txt =
  -- Building the table only pays off for longer texts
  if Text.compareLength txt 128 == GT
    then bestGreedyMatchTable pat txt
    else bestGreedyMatchScan pat txt


{-|
Implementation of 'bestGreedyMatch' that scans
the text from each start position.
Fast for short texts, but O(n²) in the worst case.
-}
bestGreedyMatchScan :: Text -> Text -> Maybe (Int, [Int])
bestGreedyMatchScan pat txt = do
  (p, ps) <- Text.uncons pat
  let
    -- Score of greedily matching the pattern's tail,
    -- given the score and run value after matching its head
    scoreFrom :: Int -> Int -> Text -> Text -> Maybe Int
    scoreFrom !tot !cur pat' txt' = case Text.uncons pat' of
      Nothing -> Just tot
      Just (x, xs) -> case Text.uncons txt' of
        Nothing -> Nothing
        Just (c, cs)
          | x == c -> let cur' = cur * 2 + 1 in scoreFrom (tot + cur') cur' xs cs
          | otherwise -> scoreFrom tot 0 pat' cs

    positionsFrom :: Int -> Text -> Text -> [Int]
    positionsFrom !idx pat' txt' = case (Text.uncons pat', Text.uncons txt') of
      (Just (x, xs), Just (c, cs))
        | x == c -> idx : positionsFrom (idx + 1) xs cs
        | otherwise -> positionsFrom (idx + 1) pat' cs
      _ -> []

    findBest :: Int -> Text -> Maybe (Int, Int) -> Maybe (Int, Int)
    findBest !idx txt' best = case Text.uncons txt' of
      Nothing -> best
      Just (c, cs)
        | c /= p -> findBest (idx + 1) cs best
        | otherwise -> case scoreFrom 1 1 ps cs of
            Nothing -> best
            Just sc ->
              findBest (idx + 1) cs $ case best of
                Just (_, bestSc) | bestSc >= sc -> best
                _ -> Just (idx, sc)

  (start, sc) <- findBest 0 txt Nothing
  pure (sc, positionsFrom start pat (Text.drop start txt))


{-|
Implementation of 'bestGreedyMatch' that uses a table
with the next occurrence of each pattern character
at or after each position, so that greedily matching from a start
takes one lookup per pattern character instead of a scan of the text.
O(n * m) for a text of length n and a pattern of length m.
-}
bestGreedyMatchTable :: Text -> Text -> Maybe (Int, [Int])
bestGreedyMatchTable pat txt = do
  let
    patLen = Text.length pat
    txtLen = Text.length txt
    patArr :: UArray Int Char
    patArr = listArray (0, patLen - 1) (Text.unpack pat)
    txtArr :: UArray Int Char
    txtArr = listArray (0, txtLen - 1) (Text.unpack txt)

    -- next ! (j * (txtLen + 1) + i): Position of the next occurrence of
    -- the j-th pattern character at or after position i (txtLen if none)
    next :: UArray Int Int
    next = runSTUArray $ do
      table <- newArray (0, patLen * (txtLen + 1) - 1) txtLen
      forM_ [txtLen - 1, txtLen - 2 .. 0] $ \i ->
        forM_ [0 .. patLen - 1] $ \j -> do
          let row = j * (txtLen + 1)
          if unsafeAt patArr j == unsafeAt txtArr i
            then unsafeWrite table (row + i) i
            else unsafeRead table (row + i + 1) >>= unsafeWrite table (row + i)
      pure table
    nextOf j i = unsafeAt next (j * (txtLen + 1) + i)

    -- Score of the greedy match starting at the given position
    -- (which must contain the first pattern character)
    scoreFrom :: Int -> Maybe Int
    scoreFrom start = go 1 start 1 1
      where
        go :: Int -> Int -> Int -> Int -> Maybe Int
        go !j !prev !tot !cur
          | j == patLen = Just tot
          | otherwise =
              let pos = nextOf j (prev + 1)
              in  if pos == txtLen
                    then Nothing
                    else
                      let cur' = if pos == prev + 1 then cur * 2 + 1 else 1
                      in  go (j + 1) pos (tot + cur') cur'

    positionsFrom :: Int -> [Int]
    positionsFrom start =
      start : go 1 start
      where
        go j prev
          | j == patLen = []
          | otherwise = let pos = nextOf j (prev + 1) in pos : go (j + 1) pos

    findBest :: Int -> Maybe (Int, Int) -> Maybe (Int, Int)
    findBest !start best
      | start == txtLen = best
      | otherwise = case scoreFrom start of
          Nothing -> best
          Just sc ->
            findBest (nextOf 0 (start + 1)) $ case best of
              Just (_, bestSc) | bestSc >= sc -> best
              _ -> Just (start, sc)

  (start, sc) <- findBest (nextOf 0 0) Nothing
  pure (sc, positionsFrom start)


{-|
Render the text, wrapping the characters
at the given (ascending) positions in pre and post.
-}
renderAt :: (T.TextualMonoid text) => (text, text) -> Text -> [Int] -> text
renderAt (pre, post) txt0 positions =
  -- Concatenate all chunks at once to avoid copying the rest repeatedly
  mconcat (go 0 txt0 positions)
  where
    go _ txt [] = [T.fromText txt]
    go offset txt (pos : rest) =
      let
        (gap, fromPos) = Text.splitAt (pos - offset) txt
        (char, after) = Text.splitAt 1 fromPos
      in
        T.fromText gap
          : pre
          : T.fromText char
          : post
          : go (pos + 1) after rest


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
match ::
  (T.TextualMonoid text) =>
  -- | Handle or ignore case of search text
  CaseSensitivity ->
  -- | Text to add before and after each match
  (text, text) ->
  -- | Function to extract the text from the container
  (value -> text) ->
  -- | Pattern
  text ->
  -- | Value containing the text to search in
  value ->
  -- | Original value, rendered string, and score
  Maybe (Fuzzy value text)
match caseSen preAndPost extract pat value = do
  let
    -- Non-character factors are ignored
    txt = T.toText (const mempty) (extract value)
    norm = if caseSen == HandleCase then identity else Text.map lowerChar
    -- Mapping single characters keeps the positions aligned with `txt`
    txtNorm = norm txt
    patNorm = norm (T.toText (const mempty) pat)
    patLen = Text.length patNorm
    (beforeSub, fromSub) = Text.breakOn patNorm txtNorm

  (sc, positions) <-
    if
      | Text.null patNorm -> Just (0, [])
      -- A contiguous match has the highest possible score
      -- and `breakOn` finds the earliest one
      | not (Text.null fromSub) ->
          let start = Text.length beforeSub
          in  Just (contiguousScore patLen, [start .. start + patLen - 1])
      | not (isSubsequenceOf patNorm txtNorm) -> Nothing
      | otherwise -> bestGreedyMatch patNorm txtNorm

  Just
    Fuzzy
      { original = value
      , rendered = renderAt preAndPost txt positions
      , score = sc
      }


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
filter ::
  (T.TextualMonoid text) =>
  -- | Handle or ignore case of search text
  CaseSensitivity ->
  -- | Text to add before and after each match
  (text, text) ->
  -- | Function to extract the text from the container
  (value -> text) ->
  -- | Pattern
  text ->
  -- | List of values containing the text to search in
  [value] ->
  -- | List of results, sorted, highest score first
  [Fuzzy value text]
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
simpleFilter ::
  (T.TextualMonoid text) =>
  -- | Pattern to look for.
  text ->
  -- | List of texts to check.
  [text] ->
  -- | The ones that match.
  [text]
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
