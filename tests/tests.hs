module Main where

import Protolude (
  Bool (False, True),
  IO,
  Maybe (Just, Nothing),
  Monad (return),
  Ord ((>)),
  head,
  identity,
  map,
  ($),
  (<$>),
 )

import Test.HUnit (Assertion, Test (..), runTestTT, (@?=))
import Text.Fuzzily as Fu (
  CaseSensitivity (HandleCase, IgnoreCase),
  Fuzzy (original, rendered, score),
  filter,
  match,
  test,
 )


from :: [Assertion] -> Test
from xs = TestList (map TestCase xs)


tests :: Test
tests =
  TestList
    [ TestLabel "test" $
        TestList
          [ TestLabel "should return true when fuzzy match" $
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
              "should return a greater score for consecutive matches of pattern"
              $ from
                [ (>)
                    (Fu.score <$> Fu.match IgnoreCase ("", "") identity "abcd" "zabcd")
                    (Fu.score <$> Fu.match IgnoreCase ("", "") identity "abcd" "azbcd")
                    @?= True
                ]
          , TestLabel
              "should return the string as is if no pre/post and case sensitive"
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
          , TestLabel "should return Nothing on no match" $
              from
                [ Fu.match
                    IgnoreCase
                    ("", "")
                    identity
                    "ZEBRA!"
                    "ZaZbZ"
                    @?= Nothing
                ]
          , TestLabel "should be case sensitive is specified" $
              from
                [ Fu.match
                    HandleCase
                    ("", "")
                    identity
                    "hask"
                    "Haskell"
                    @?= Nothing
                ]
          , TestLabel "should be wrap pre and post around matches" $
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
          [ TestLabel "should return list untouched when given empty pattern" $
              from
                [ map
                    Fu.original
                    (Fu.filter HandleCase ("", "") identity "" ["abc", "def"])
                    @?= ["abc", "def"]
                ]
          , TestLabel "should return the highest score first" $
              from
                [ (@?=)
                    (head (Fu.filter HandleCase ("", "") identity "cb" ["cab", "acb"]))
                    (head (Fu.filter HandleCase ("", "") identity "cb" ["acb"]))
                ]
          ]
    ]


runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()


-- | For now, main will run our tests.
main :: IO ()
main = runTests
