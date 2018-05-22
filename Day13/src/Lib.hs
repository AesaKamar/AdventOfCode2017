{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Arrow (second)
import qualified Data.Array as Array
import qualified Data.Array.IArray as IArray
import qualified Data.Ix as Ix
import Data.List (find)
import Data.Semigroup ((<>))
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number (int)

type IndexX = Integer

type IndexY = Integer

type Size = Integer

data Column =
  Column IndexX
         Size

moveSecurityBot :: Size -> Integer -> IndexY
moveSecurityBot lengthOfArray picoseconds =
  let splength = ((lengthOfArray - 1) * 2)
      pos = picoseconds `mod` splength
  in if pos < lengthOfArray
       then pos
       else splength - pos

moveScanner :: [IndexX] -> Integer -> [Maybe Column] -> [IndexX]
moveScanner stucks pcs [] = stucks
moveScanner stucks pcs (Nothing:restOfColumns) =
  moveScanner stucks (succ pcs) restOfColumns
moveScanner stucks pcs (Just (Column x len):restOfColumns) =
  let securityBotPos = moveSecurityBot len pcs
      scannerPos = 0
      nextPcs = (succ pcs)
  in if scannerPos /= securityBotPos
       then moveScanner stucks nextPcs restOfColumns
       else moveScanner (stucks <> pure x) nextPcs restOfColumns

makeIndexedList :: forall a. [(Integer, a)] -> [Maybe a]
makeIndexedList [] = []
makeIndexedList elems =
  let arrayOfNothing =
        Array.array (0, 100) ((\i -> (i, Nothing)) <$> [0 .. 100])
      arrayWithSomething = arrayOfNothing Array.// (second Just <$> elems)
  in IArray.elems arrayWithSomething

findColumnsByIndex :: [Column] -> [IndexX] -> [Maybe Column]
findColumnsByIndex cols idxs =
  fmap (\ix -> (find (\(Column cx cs) -> cx == ix) cols)) idxs

calculateSeverity :: Column -> Integer
calculateSeverity (Column idxx size) = idxx * size

columnParser :: Parser Column
columnParser = do
  num1 <- int
  string ": "
  num2 <- int
  pure $ Column num1 num2
