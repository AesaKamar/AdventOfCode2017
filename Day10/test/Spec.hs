{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Data.List              as L
import qualified Data.Vector.Persistent as V
import           Lib
import           Test.Tasty.Hspec
import           Text.Show.Pretty

main :: IO ()
main =
  hspec $
  -- describe "Flipping a string" $ do
  --   it "should tie with a single number" $
  --     (tieKnot (V.fromList [0..4]) 0 3) `shouldBe`  V.fromList [2, 1, 0, 3, 4]
  --   it "should tie with a single number with offset" $
  --     (tieKnot (V.fromList [0..4]) 1 3) `shouldBe`  V.fromList [0, 3, 2, 1, 4]
  --   it "should tie with a single number with offset" $
  --     (tieKnot (V.fromList [0..4]) 2 3) `shouldBe`  V.fromList [0, 1, 4, 3, 2]
  --   it "should tie with a single number with offset" $
  --     (tieKnot (V.fromList [0..4]) 3 3) `shouldBe`  V.fromList [3, 1, 2, 0, 4]
   do
    describe "partitioning a string" $ do
      t1
      t2
      t3
      t4
      t5
  where
    t1 =
      let myVec = ([0 .. 4] `zip` [0 .. 4])
      in it "partitionGroups 0 5 [0..4]" $
         (partitionGroups 0 5 myVec) `shouldBe` (myVec, mempty)
    t2 =
      let myVec = ([0 .. 4] `zip` [0 .. 4])
      in it "partitionGroups 0 0 [0..4]" $
         (partitionGroups 0 0 myVec) `shouldBe` (mempty, myVec)
    t3 =
      let myVec = ([0 .. 4] `zip` [0 .. 4])
          excludedSet = ([1, 2] `zip` [1, 2])
          includedSet = ([3, 4, 0] `zip` [3, 4, 0])
      in it "partitionGroups 3 3 [0..4]" $
         (partitionGroups 3 3 myVec) `shouldBe` (includedSet, excludedSet)
    t4 =
      let myVec = ([0 .. 5] `zip` [0 .. 5])
          excludedSet = ([2, 3] `zip` [2, 3])
          includedSet = ([4, 5, 0, 1] `zip` [4, 5, 0, 1])
      in it "partitionGroups 4 4 [0..5]" $
         (partitionGroups 4 4 myVec) `shouldBe` (includedSet, excludedSet)
    t5 =
      let myVec = ([0 .. 5] `zip` [0 .. 5])
          excludedSet = ([4, 5, 0, 1, 2] `zip` [4, 5, 0, 1, 2])
          includedSet = ([3] `zip` [3])
      in it "partitionGroups 3 1 [0..5]" $
         (partitionGroups 3 1 myVec) `shouldBe` (includedSet, excludedSet)
