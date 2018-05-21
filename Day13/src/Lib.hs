{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where


import qualified Data.Array as Array
import qualified Data.Ix    as Ix


type IndexX = Integer
type IndexY = Integer
type Size = Integer




data Column = Column IndexX Size




moveSecurityBot :: Integer -> Size -> IndexY
moveSecurityBot lengthOfArray picoseconds =
  let splength = ((lengthOfArray-1) *2)
      pos = picoseconds `mod` splength
  in
    if (pos < lengthOfArray) then pos
    else splength - pos
