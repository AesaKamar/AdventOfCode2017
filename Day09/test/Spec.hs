module Main where

import           Control.Arrow          (left)
import           Control.Exception      (evaluate)
import           Lib
import           Test.Tasty.Hspec
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

instance Eq ParseError where
  (==) _ _ = True

main :: IO ()
main =
  hspec $ do
    describe "Looking for garbage" $ do
      shouldParseAs "<>" parseGarbage (Garbage "")
      shouldParseAs
        "<random characters>"
        parseGarbage
        (Garbage "random characters")
      shouldParseAs "<<<<>" parseGarbage (Garbage "<<<")
      shouldParseAs "<!>>" parseGarbage (Garbage "!>")
      shouldParseAs "<{!>}>" parseGarbage (Garbage "{!>}")
      shouldParseAs "<!!>" parseGarbage (Garbage "!!")
      shouldParseAs "<!!!>>" parseGarbage (Garbage "!!!>")
      shouldParseAs "<{o\"i!a,<{i<a>" parseGarbage (Garbage "{o\"i!a,<{i<a")
    describe "Looking for groups" $ do
      shouldParseAs "{}" parseGroups (Group [])
      shouldParseAs "{{{}}}" parseGroups (Group [Group [Group []]])
      shouldParseAs "{{},{}}" parseGroups (Group [Group [], Group []])
      shouldParseAs
        "{{{},{},{{}}}}"
        parseGroups
        (Group [Group [Group [], Group [], Group [Group []]]])
      shouldParseAs "{<{},{},{{}}>}" parseGroups (Group [Garbage "{},{},{{}}"])
      shouldParseAs
        "{<a>,<a>,<a>,<a>}"
        parseGroups
        (Group (replicate 4 (Garbage "a")))
      shouldParseAs
        "{{<a>},{<a>},{<a>},{<a>}}"
        parseGroups
        (Group (replicate 4 (Group [Garbage "a"])))
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
    describe "Counting nonescaped chars" $ do
      "<>" `shouldCountNonEscapedCharsAs` 0
      "<random characters>" `shouldCountNonEscapedCharsAs` 17
      "<<<<>" `shouldCountNonEscapedCharsAs` 3
      "<{!>}>" `shouldCountNonEscapedCharsAs` 2
      "<!!>" `shouldCountNonEscapedCharsAs` 0
      "<!!!>>" `shouldCountNonEscapedCharsAs` 0
      "<{o\"i!a,<{i<a>" `shouldCountNonEscapedCharsAs` 10

-- Wow these are magical
-- ~Type Inference~
shouldScoreAs inputString expectation =
  it inputString $
  (scoreGroups 1 <$> (parse parseGroups "" inputString)) `shouldBe`
  (Right expectation)

shouldCountAs inputString expectation =
  it inputString $
  (countGroups <$> (parse parseGroups "" inputString)) `shouldBe`
  (Right expectation)

shouldCountNonEscapedCharsAs inputString expectation =
  let removeFirstLast xs = (tail . init) xs
      minusTwoOuters = removeFirstLast inputString
  in it inputString $
     (countNonEscapedChars minusTwoOuters) `shouldBe` expectation

shouldParseAs inputString parser expectation =
  it inputString $ parse parser "" inputString `shouldBe` (Right expectation)
