module Main where

import Protolude (
  Bool (False, True),
  Char,
  Eq ((/=), (==)),
  IO,
  Int,
  Integral (div, mod),
  Maybe (Just, Nothing),
  Monad ((>>=)),
  Monoid (mempty),
  Num ((*), (+)),
  Ord ((<=), (>)),
  Text,
  drop,
  evaluate,
  fst,
  head,
  identity,
  iterate,
  map,
  not,
  replicateM,
  take,
  toLower,
  when,
  ($),
  (<$>),
  (<>),
 )

import Data.Text qualified as T
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Test.HUnit (
  Assertion,
  Counts (errors, failures),
  Test (..),
  assertBool,
  runTestTT,
  (@?=),
 )
import Text.Fuzzily as Fu (
  CaseSensitivity (HandleCase, IgnoreCase),
  Fuzzy (Fuzzy, original, rendered, score),
  bestGreedyMatchScan,
  bestGreedyMatchTable,
  filter,
  isSubsequenceOf,
  match,
  simpleFilter,
  test,
 )


from :: [Assertion] -> Test
from xs = TestList (map TestCase xs)


tests :: [Test]
tests =
  [ TestLabel "test" $
      TestList
        [ TestLabel "returns true when fuzzy match" $
            from
              [ Fu.test "back" "imaback" @?= True
              , Fu.test "back" "bakck" @?= True
              , Fu.test "shig" "osh kosh modkhigow" @?= True
              , Fu.test "" "osh kosh modkhigow" @?= True
              ]
        , TestLabel "should return false when no fuzzy match" $
            from
              [ Fu.test "back" "abck" @?= False
              , Fu.test "okmgk" "osh kosh modkhigow" @?= False
              ]
        ]
  , TestLabel "match" $
      TestList
        [ TestLabel
            "returns a greater score for continuous matches of pattern"
            $ from
            $ let
                scoreContinuousMatch =
                  Fu.score
                    <$> Fu.match IgnoreCase ("", "") identity "abcd" "zabcd"
                scoreSplitMatch =
                  Fu.score
                    <$> Fu.match IgnoreCase ("", "") identity "abcd" "azbcd"
              in
                [ scoreContinuousMatch @?= Just 26
                , scoreSplitMatch @?= Just 12
                ]
        , TestLabel
            "returns the longest continuous match, even if it's at the end"
            $ from
            $ let
                actual =
                  Fu.rendered
                    <$> Fu.match
                      IgnoreCase
                      ("<", ">")
                      identity
                      "abcd"
                      "This is a sizeable text with a final matching word: abcd"
                expected =
                  Just $
                    "This is a sizeable text"
                      <> " with a final matching word: <a><b><c><d>"
              in
                [actual @?= expected]
        , TestLabel
            "returns the string as is if no pre/post and case sensitive"
            $ from
              [ Fu.rendered
                  <$> Fu.match
                    HandleCase
                    ("", "")
                    identity
                    "ab"
                    "ZaZbZ"
                  @?= Just "ZaZbZ"
              ]
        , TestLabel "returns Nothing on no match" $
            from
              [ Fu.match
                  IgnoreCase
                  ("", "")
                  identity
                  "ZEBRA!"
                  "ZaZbZ"
                  @?= Nothing
              ]
        , TestLabel "is case sensitive if specified" $
            from
              [ Fu.match
                  HandleCase
                  ("", "")
                  identity
                  "hask"
                  "Haskell"
                  @?= Nothing
              ]
        , TestLabel "wraps pre and post around matches" $
            from
              [ Fu.rendered
                  <$> Fu.match
                    HandleCase
                    ("<", ">")
                    identity
                    "brd"
                    "bread"
                  @?= Just "<b><r>ea<d>"
              ]
        ]
  , TestLabel "filter" $
      TestList
        [ TestLabel "returns list untouched when given empty pattern" $
            from
              [ ( Fu.original
                    `map` Fu.filter
                      HandleCase
                      ("", "")
                      identity
                      ""
                      ["abc", "def"]
                )
                  @?= ["abc", "def"]
              ]
        , TestLabel "returns the highest score first" $
            from
              [ (@?=)
                  ( head $
                      Fu.filter
                        HandleCase
                        ("", "")
                        identity
                        "cb"
                        ["cab", "acb"]
                  )
                  (head $ Fu.filter HandleCase ("", "") identity "cb" ["acb"])
              ]
        , TestLabel "keeps original casing when filtering case insensitive" $
            from
              [ ( Fu.original
                    `map` ( Fu.filter
                              IgnoreCase
                              ("", "")
                              identity
                              "abc"
                              ["aBc"]
                          )
                )
                  @?= ["aBc"]
              ]
        ]
  , TestLabel "README examples" $
      TestList
        [ TestLabel "hsk" $
            from
              [ ( match
                    IgnoreCase
                    ("<", ">")
                    fst
                    "hsk"
                    ("Haskell", 1995 :: Int)
                )
                  @?= Just
                    ( Fuzzy
                        { original = ("Haskell", 1995)
                        , rendered = "<H>a<s><k>ell"
                        , score = 5
                        }
                    )
              ]
        , TestLabel "langs" $
            from
              [ let
                  langs :: [([Char], Int)]
                  langs =
                    [ ("Standard ML", 1990)
                    , ("OCaml", 1996)
                    , ("Scala", 2003)
                    ]
                  result = filter IgnoreCase ("<", ">") fst "ML" langs
                  expected =
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
                in
                  result @?= expected
              ]
        , TestLabel "simple filter" $
            from
              [ simpleFilter "vm" ["vim", "emacs", "virtual machine"]
                  @?= ["vim", "virtual machine"]
              ]
        , TestLabel "test" $
            from
              [test "brd" "bread" @?= True]
        ]
  ]


{-|
Reference implementation of the original (cubic) algorithm
to verify that the optimized one yields identical results.
-}
referenceMatch ::
  CaseSensitivity -> (Text, Text) -> Text -> Text -> Maybe (Text, Int)
referenceMatch caseSen (pre, post) pat txt0 =
  go mempty txt0 Nothing
  where
    norm = if caseSen == HandleCase then identity else toLower
    matchOnce txt =
      let
        (tot, _, res, restPat) =
          T.foldl'
            ( \(tot_, cur, acc, p) c -> case T.uncons p of
                Nothing -> (tot_, 0 :: Int, acc <> T.singleton c, p)
                Just (x, xs)
                  | norm x == norm c ->
                      let cur' = cur * 2 + 1
                      in  (tot_ + cur', cur', acc <> pre <> T.singleton c <> post, xs)
                  | True -> (tot_, 0, acc <> T.singleton c, p)
            )
            (0, 0, mempty, pat)
            txt
      in
        if T.null restPat then Just (res, tot) else Nothing
    go pref txt best =
      let
        best' = case matchOnce txt of
          Just (rendSub, sc) -> case best of
            Just (_, bestSc) | sc <= bestSc -> best
            _ -> Just (pref <> rendSub, sc)
          Nothing -> best
      in
        case T.uncons txt of
          Nothing -> best'
          Just (c, rest) -> go (pref <> T.singleton c) rest best'


-- | All strings over the given alphabet up to the given length
stringsUpTo :: Int -> [Char] -> [Text]
stringsUpTo n alphabet =
  [0 .. n] >>= \k -> T.pack <$> replicateM k alphabet


equivalenceTests :: Test
equivalenceTests =
  TestLabel "matches the reference implementation" $
    from
      [ [ (caseSen, pat, txt)
        | let tags = (T.pack "<", T.pack ">")
        , caseSen <- [IgnoreCase, HandleCase]
        , pat <- stringsUpTo 4 "aBc"
        , txt <- stringsUpTo 6 "abC"
        , ((\f -> (rendered f, score f)) <$> Fu.match caseSen tags identity pat txt)
            /= referenceMatch caseSen tags pat txt
        ]
          @?= []
      ]


-- | Pseudo-random text over the given alphabet (linear congruential generator)
pseudoRandomText :: Int -> Int -> Text -> Text
pseudoRandomText seed len alphabet =
  T.pack $
    map
      (\x -> T.index alphabet ((x `div` 65536) `mod` T.length alphabet))
      ( take
          len
          (drop 1 (iterate (\x -> (x * 1103515245 + 12345) `mod` 2147483648) seed))
      )


greedyImplementationTests :: Test
greedyImplementationTests =
  TestLabel "scan and table implementations agree" $
    from
      [ [ (pat, txt)
        | (pat, txt) <-
            [(pat, txt) | pat <- stringsUpTo 4 "abc", txt <- stringsUpTo 6 "abc"]
              <> [ (pat, pseudoRandomText seed (150 + seed) (T.pack "abcd"))
                 | seed <- [1 .. 250]
                 , pat <- stringsUpTo 3 "abcd"
                 ]
        , not (T.null pat)
        , Fu.isSubsequenceOf pat txt
        , Fu.bestGreedyMatchScan pat txt /= Fu.bestGreedyMatchTable pat txt
        ]
          @?= []
      ]


performanceTests :: Test
performanceTests =
  TestLabel "matches long texts quickly" $
    from
      [ do
          let
            longText =
              T.replicate 2000 (T.pack "lorem ipsum ")
                <> T.pack "xyz"
                <> T.replicate 2000 (T.pack " dolor sit")
          result <-
            timeout 2000000 $
              evaluate $
                (\f -> (score f, T.length (rendered f)))
                  <$> Fu.match
                    IgnoreCase
                    (T.pack "<", T.pack ">")
                    identity
                    (T.pack "xyz")
                    longText
          assertBool "match took longer than 2 seconds" $
            result == Just (Just (11, T.length longText + 6))
      ]


runTests :: IO ()
runTests = do
  counts <-
    runTestTT $
      TestList
        (tests <> [equivalenceTests, greedyImplementationTests, performanceTests])
  when (errors counts + failures counts > 0) exitFailure


-- | For now, main will run our tests.
main :: IO ()
main = runTests
