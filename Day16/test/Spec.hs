module Main
where

import           Data.Array
import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = hspec $ do
  describe "spin" $ do
    it "s0 abcde -> abcde"
      $          show
                   (spin
                     Structure {cursor = 0, len = 5, letters = listArray (0, 4) [A .. E]}
                     0
                   )
      `shouldBe` "abcde"
    it "s1 abcde -> eabcd"
      $          show
                   (spin
                     Structure {cursor = 0, len = 5, letters = listArray (0, 4) [A .. E]}
                     1
                   )
      `shouldBe` "eabcd"
    it "s3 abcde -> cdeab"
      $          show
                   (spin
                     Structure {cursor = 0, len = 5, letters = listArray (0, 4) [A .. E]}
                     3
                   )
      `shouldBe` "cdeab"
  describe "exchange" $ do
    it "x3/4 eabcd -> eabdc"
      $          show
                   (exchange
                     Structure
                       { cursor  = 0
                       , len     = 5
                       , letters = listArray (0, 4) [E, A, B, C, D]
                       }
                     (3, 4)
                   )
      `shouldBe` "eabdc"
  describe "partner" $ do
    it "pe/b eabdc -> baedc"
      $          show
                   (partner
                     Structure
                       { cursor  = 0
                       , len     = 5
                       , letters = listArray (0, 4) [E, A, B, D, C]
                       }
                     (E, B)
                   )
      `shouldBe` "baedc"
