module Day6 where

import           Data.List  (elemIndex, findIndex, replicate)
import           Data.Maybe (fromMaybe)

-- maximumMaybe :: Ord a => [a] -> Maybe a
-- maximumMaybe [] = Nothing
-- maximumMaybe as = Just $ maximum as
--
-- indexOfMaximum :: (Ord a) => [a] -> Maybe Int
-- indexOfMaximum as = do
--   mx <- maximumMaybe as
--   idx <- elemIndex mx as
--   return idx
placeNAtPos :: Int -> Int -> [Int] -> [Int]
placeNAtPos n pos elems = firstN ++ [n] ++ lastN
  where
    firstN = take (pos) elems
    lastN = take (length elems - pos) $ drop (pos + 1) elems

initialDistribution :: Int -> Int -> Int
initialDistribution size numToDist = numToDist `div` size

-- 0 | 1 | 2 | 3 | 4 index
-- 2 | 3 | 4 | 0 | 1 relIdx
-- 0 | 0 | 0 | 4 | 0 vals
-- 1 | 1 | 1 | 0 | 1 mapped
remainingDistribution :: Int -> Int -> Int -> Int -> Int
remainingDistribution size numToDist initialPos pos =
  if relativePos < remainingToDist
    then 0
    else 1
  where
    relativePos = (pos - initialPos) `mod` size
    remainder = size `mod` numToDist
    remainingToDist = remainder

performDistribution :: [Int] -> [Int]
performDistribution slots =
  let maxValue = maximum slots
      maxIndex = fromMaybe 0 (elemIndex maxValue slots)
      size = length slots
      listSansMax = placeNAtPos 0 maxIndex slots
      firstPass = replicate size (initialDistribution size maxValue)
      remainderPass =
        map (remainingDistribution size maxValue maxIndex) [0 .. size - 1]
      zipped = zip3 listSansMax firstPass remainderPass
  in map (\(x, y, z) -> x + y + z) zipped

testThing1 = map (\x -> initialDistribution 4 7) [0 .. 3]

testThing2 = map (remainingDistribution 4 3 2) [0 .. 3]

testThing3 = performDistribution [0, 0, 0, 3, 0, 0]

testThing4 = performDistribution [0, 2, 7, 0]
