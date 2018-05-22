module Lib where

import qualified Data.Array     as Array
import qualified Data.Ix        as Ix
import           Data.Semigroup ((<>))


type IndexX = Integer
type IndexY = Integer
type Size = Integer




data Column = Column IndexX Size


moveSecurityBot :: Size -> Integer -> IndexY
moveSecurityBot lengthOfArray picoseconds =
  let splength = ((lengthOfArray-1) *2)
      pos = picoseconds `mod` splength
  in
    if pos < lengthOfArray
    then pos
    else splength - pos


moveScanner :: [IndexX] -> Integer -> [Maybe Column] -> [IndexX]
moveScanner stucks pcs [] = stucks
moveScanner stucks pcs (Nothing : restOfColumns) =
  moveScanner stucks (succ pcs) restOfColumns
moveScanner stucks  pcs (Just (Column x len) : restOfColumns) =
  let
    securityBotPos = moveSecurityBot len pcs
    scannerPos = 0
    nextPcs = (succ pcs)
  in
    if scannerPos /= securityBotPos
    then moveScanner stucks nextPcs restOfColumns
    else moveScanner (stucks <> pure x) nextPcs restOfColumns
