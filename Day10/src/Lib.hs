module Lib  where

import           Data.Monoid

lengthOfNumberString :: Int
lengthOfNumberString = 256

numberString :: [Int]
numberString = [0..255]


tieKnot :: [Int] -> Int -> Int -> [Int]
tieKnot numStr currentIndex skipAmount =
  let
    (first, second) = splitAt skipAmount numStr
  in reverse first <> second
