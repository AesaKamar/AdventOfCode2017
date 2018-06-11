module Lib where

import qualified Data.Boolean           as B
import qualified Data.List              as L
import           Data.Monoid
import qualified Data.Vector.Persistent as V

lengthOfNumberString :: Int
lengthOfNumberString = 256

numberString :: V.Vector Int
numberString = V.fromList [0 .. 255]

applyNTimes n f xs = iterate f xs !! n

-- tieKnot :: V.Vector Int -> Int -> Int -> V.Vector Int
-- tieKnot numStr offset numToTake =
--   let endOfGroup = (offset + numToTake)
--       endOfNumStr = (V.length numStr)
--       reversePart = V.reverse (V.slice offset endOfGroup numStr)
--       theRest = V.slice endOfGroup endOfNumStr numStr
--   in reversePart <> theRest
indexBetween beginning ending len (idx, _) =
  let cycledEnding =
        if ending > len
          then ending `mod` len
          else ending
  in if cycledEnding == beginning
       then False
       else if cycledEnding > beginning
              then (idx <= cycledEnding) B.&& (idx >= beginning)
              else B.not $ (idx < cycledEnding) `B.xor` (idx > beginning)

-- Returns (IncludedInGroup, ExcludedInGroup)
-- For some reason, the included group is already reversed
partitionGroups :: Int -> Int -> [] (Int, a) -> ([] (Int, a), [] (Int, a))
partitionGroups offset groupSize xs =
  L.partition (indexBetween from to (L.length xs)) xs
  where
    from = offset
    to = offset + groupSize
