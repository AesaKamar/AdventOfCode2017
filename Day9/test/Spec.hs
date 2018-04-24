module Main where

import           Control.Exception      (evaluate)
import           Lib
import           Test.Tasty.Hspec
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String



instance Eq ParseError where
  (==) _ _ = False

main :: IO ()
main = hspec $ do
  describe "Lookin for garbage" $ do
    shouldParseAs "<>" parserGarbage (Right Garbage)
    shouldParseAs "<random characters>" parserGarbage (Right Garbage)
    shouldParseAs "<<<<>" parserGarbage (Right Garbage)
    shouldParseAs "<{!>}>" parserGarbage (Right Garbage)
    shouldParseAs "<!!>" parserGarbage (Right Garbage)
    shouldParseAs "<!!!>>" parserGarbage (Right Garbage)
    shouldParseAs "<{o\"i!a,<{i<a>" parserGarbage (Right Garbage)





shouldParseAs inputString parser expectation =
  it inputString $ parse parser "" inputString `shouldBe` expectation
