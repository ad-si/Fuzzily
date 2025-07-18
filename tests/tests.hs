module Main where

import Protolude (
  Bool (False, True),
  Char,
  IO,
  Int,
  Maybe (Just, Nothing),
  Monad (return),
  fst,
  head,
  identity,
  map,
  ($),
  (<$>),
  (<>),
 )

import Test.HUnit (Assertion, Test (..), runTestTT, (@?=))
import Text.Fuzzily as Fu (
  CaseSensitivity (HandleCase, IgnoreCase),
  Fuzzy (Fuzzy, original, rendered, score),
  filter,
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


runTests :: IO ()
runTests = do
  _ <- runTestTT $ TestList tests
  return ()


-- | For now, main will run our tests.
main :: IO ()
main = runTests
