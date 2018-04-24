module Main where

import Control.Arrow (left)
import Control.Exception (evaluate)
import Lib
import Test.Tasty.Hspec
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String

instance Eq ParseError where
  (==) _ _ = True

main :: IO ()
main =
  hspec $ do
    describe "Looking for garbage" $ do
      shouldParseAs "<>" parserGarbage (Right (Garbage ""))
      shouldParseAs
        "<random characters>"
        parserGarbage
        (Right (Garbage "random characters"))
      shouldParseAs "<<<<>" parserGarbage (Right $ Garbage "<<<")
      shouldParseAs "<!>>" parserGarbage (Right $ Garbage "!>")
      shouldParseAs "<{!>}>" parserGarbage (Right $ Garbage "{!>}")
      shouldParseAs "<!!>" parserGarbage (Right $ Garbage "!!")
      shouldParseAs "<!!!>>" parserGarbage (Right $ Garbage "!!!>")
      shouldParseAs
        "<{o\"i!a,<{i<a>"
        parserGarbage
        (Right $ Garbage "{o\"i!a,<{i<a")
    describe "Looking for groups" $ do
      shouldParseAs "{}" parseGroups (Right $ Group [])
      shouldParseAs "{{{}}}" parseGroups (Right $ Group [Group [Group []]])
      shouldParseAs "{{},{}}" parseGroups (Right $ Group [Group [], Group []])
      shouldParseAs
        "{{{},{},{{}}}}"
        parseGroups
        (Right $ Group [Group [Group [], Group [], Group [Group []]]])
      shouldParseAs
        "{<{},{},{{}}>}"
        parseGroups
        (Right $ Group [Garbage "{},{},{{}}"])
      shouldParseAs
        "{<a>,<a>,<a>,<a>}"
        parseGroups
        (Right $ Group (replicate 4 (Garbage "a")))
      shouldParseAs
        "{{<a>},{<a>},{<a>},{<a>}}"
        parseGroups
        (Right $ Group (replicate 4 (Group [Garbage "a"])))
    describe "Counting groups" $ do
      "{}" `shouldCountAs` 1
      "{{{}}}" `shouldCountAs` 3
      "{{},{}}" `shouldCountAs` 3
      "{{{},{},{{}}}}" `shouldCountAs` 6
      "{<{},{},{{}}>}" `shouldCountAs` 1
      "{<a>,<a>,<a>,<a>}" `shouldCountAs` 1
      "{{<a>},{<a>},{<a>},{<a>}}" `shouldCountAs` 5
      "{{<!>},{<!>},{<!>},{<a>}}" `shouldCountAs` 2
    describe "Scoring groups" $ do
      "{}" `shouldScoreAs` 1
      "{{{}}}" `shouldScoreAs` 6
      "{{},{}}" `shouldScoreAs` 5
      "{{{},{},{{}}}}" `shouldScoreAs` 16
      "{<a>,<a>,<a>,<a>}" `shouldScoreAs` 1
      "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldScoreAs` 9
      "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldScoreAs` 9
      "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldScoreAs` 3

shouldScoreAs inputString expectation =
  it inputString $
  (scoreGroups 1 <$> (parse parseGroups "" inputString)) `shouldBe`
  (Right expectation)

shouldCountAs inputString expectation =
  it inputString $
  (countGroups <$> (parse parseGroups "" inputString)) `shouldBe`
  (Right expectation)

-- Wow this is magical
shouldParseAs inputString parser expectation =
  it inputString $ parse parser "" inputString `shouldBe` expectation
