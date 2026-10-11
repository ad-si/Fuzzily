module Main where

import Protolude (
  IO,
  Int,
  Maybe,
  Text,
  fmap,
  identity,
  length,
  map,
  show,
  ($),
  (.),
  (<>),
 )

import Data.Text qualified as T
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Text.Fuzzily (CaseSensitivity (HandleCase, IgnoreCase), Fuzzy (..))
import Text.Fuzzily qualified as Fu


-- | Force score and rendered text
run :: CaseSensitivity -> Text -> Text -> Maybe (Int, Text)
run caseSen pat txt =
  fmap
    (\f -> (score f, rendered f))
    (Fu.match caseSen (T.pack "<", T.pack ">") identity pat txt)


-- | Number of matching texts
filterCount :: Text -> [Text] -> Int
filterCount pat =
  length . Fu.filter IgnoreCase (T.pack "<", T.pack ">") identity pat


longText :: Text
longText =
  T.replicate 1000 (T.pack "Lorem ipsum dolor sit amet ")
    <> T.pack "Schedule meeting with Alex"
    <> T.replicate 1000 (T.pack " consectetur adipiscing elit")


manyAs :: Text
manyAs = T.replicate 5000 (T.pack "a") <> T.pack "b"


shortTexts :: [Text]
shortTexts =
  map
    (\i -> T.pack ("Task number " <> show i <> " about some meeting topic"))
    [1 .. 10000 :: Int]


main :: IO ()
main =
  defaultMain
    [ bgroup
        "long text"
        [ bench "substring, ignore case" $
            nf (run IgnoreCase (T.pack "meeting")) longText
        , bench "substring, handle case" $
            nf (run HandleCase (T.pack "meeting")) longText
        , bench "fuzzy" $ nf (run IgnoreCase (T.pack "schmtal")) longText
        , bench "no match" $ nf (run IgnoreCase (T.pack "xyzqw")) longText
        , bench "single char" $ nf (run IgnoreCase (T.pack "s")) longText
        ]
    , bgroup
        "pathological"
        [ bench "ab in a…ab" $ nf (run IgnoreCase (T.pack "ab")) manyAs
        , bench "ac in a…ab" $ nf (run IgnoreCase (T.pack "ac")) manyAs
        ]
    , bgroup
        "many short texts"
        [ bench "filter substring" $
            nf
              (filterCount (T.pack "meeting"))
              shortTexts
        , bench "filter fuzzy" $
            nf
              (filterCount (T.pack "tskmtg"))
              shortTexts
        ]
    , bgroup
        "String"
        [ bench "substring" $
            nf
              (fmap score . Fu.match IgnoreCase ("<", ">") identity "meeting")
              (T.unpack longText)
        ]
    ]
