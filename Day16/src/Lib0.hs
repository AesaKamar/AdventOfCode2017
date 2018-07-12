{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib0
where

import           Control.Applicative
import           Data.Char                      ( toLower )
import           Data.Ix
import           Data.List                      ( elemIndex
                                                , groupBy
                                                , inits
                                                , tails
                                                )
import qualified Data.List.NonEmpty            as NEL
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           Data.Semigroup
import           Data.Traversable               ( sequenceA )
import           Data.Vector                    ( (!)
                                                , (//)
                                                )
import qualified Data.Vector                   as Vec
import           Text.Parsec.Char
import           Text.Parsec.String             ( Parser )
import           Text.ParserCombinators.Parsec.Number

instance Semigroup PositionMappings where
  (<>) a b = (\(startA, endA) -> (startA, snd $ b ! endA)) <$> a

type PositionMappings = Vec.Vector (Int, Int)

type LetterNameMappings = Map.Map Char Int

spin :: Int -> PositionMappings -> PositionMappings
spin offset posMapping =
  (\(startI, endI) -> (startI, cyclicShift endI)) <$> posMapping
  where cyclicShift endI = (endI - offset) `mod` length posMapping

exchange :: (Int, Int) -> PositionMappings -> PositionMappings
exchange (i1, i2) posMapping =
  posMapping // [(i1, posMapping ! i2), (i2, posMapping ! i1)]

partner :: (Char, Char) -> LetterNameMappings -> LetterNameMappings
partner (l1, l2) letMappingInit = letMappingInit Map.\\ Map.fromList
  [(l1, letMappingInit Map.! l2), (l2, letMappingInit Map.! l1)]

dance
  :: (PositionMappings, LetterNameMappings)
  -> [Move]
  -> (PositionMappings, LetterNameMappings)
dance (initPos, initLetters) [] = (initPos, initLetters)
dance (initPos, initLetters) ((LetterIndependent newPos) : rest) =
  dance (initPos <> newPos, initLetters) rest
dance (initPos, initLetters) ((LetterDependent letterMappingFn) : rest) =
  dance (initPos, letterMappingFn initLetters) rest

data Move
  = LetterIndependent PositionMappings
  | LetterDependent (LetterNameMappings -> LetterNameMappings)

emptyPositionMapping16 = Vec.fromList [ (x, x) | x <- [0 .. 15] ]

parseSpin :: Parser Move
parseSpin = do
  s <- char 's'
  n <- int
  pure $ LetterIndependent (spin n emptyPositionMapping16)

parseExchange :: Parser Move
parseExchange = do
  x     <- char 'x'
  n1    <- int
  slash <- char '/'
  n2    <- int
  pure $ LetterIndependent (exchange (n1, n2) emptyPositionMapping16)

parsePartner :: Parser Move
parsePartner = do
  p     <- char 'p'
  l1    <- letter
  slash <- char '/'
  l2    <- letter
  pure $ LetterDependent (partner (l1, l2))
