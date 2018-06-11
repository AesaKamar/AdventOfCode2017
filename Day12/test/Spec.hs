module Main where

import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec
import           Text.Parsec

instance Eq ParseError where
  (==) a b = False

main :: IO ()
main =
  hspec $
  describe "Parsing" $ do
    it "0 <-> 2" $ (parse parseLine "" "0 <-> 2") `shouldBe` Right (0, [2])
    it "2 <-> 0, 3, 4" $
      (parse parseLine "" "2 <-> 0, 3, 4") `shouldBe` Right (2, [0, 3, 4])
