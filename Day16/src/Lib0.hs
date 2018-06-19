{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib0 where

import           Control.Applicative
import           Data.Char                            (toLower)
import           Data.Ix
import           Data.List                            (elemIndex, inits, tails)
import qualified Data.List.NonEmpty                   as NEL
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust)
import           Data.Semigroup
import           Data.Traversable                     (sequenceA)
import           Data.Vector                          ((!), (//))
import qualified Data.Vector                          as Vec
import           Text.Parsec.Char
import           Text.Parsec.String                   (Parser)
import           Text.ParserCombinators.Parsec.Number

data Letter
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  deriving (Eq, Show, Enum, Read)

instance Semigroup PositionMappings where
  (<>) a b = (\(startA, endA) -> (startA, snd $ b ! endA)) <$> a

type PositionMappings = Vec.Vector (Int, Int)

type LetterNameMappings = Map.Map Letter Int

spin :: PositionMappings -> Int -> PositionMappings
spin posMapping offset =
  (\(startI, endI) -> (startI, cyclicShift endI)) <$> posMapping
  where
    cyclicShift endI = (endI - offset) `mod` length posMapping

exchange :: PositionMappings -> (Int, Int) -> PositionMappings
exchange posMapping (i1, i2) =
  posMapping // [(i1, posMapping ! i2), (i2, posMapping ! i1)]

partner :: (Letter, Letter) -> LetterNameMappings -> LetterNameMappings
partner (l1, l2) letMappingInit =
  letMappingInit Map.\\
  Map.fromList [(l1, letMappingInit Map.! l2), (l2, letMappingInit Map.! l1)]

dancePositionMappings ::
     LetterNameMappings -> NEL.NonEmpty PositionMappings -> LetterNameMappings
dancePositionMappings lm pms = (\x -> snd $ finalPm ! x) <$> lm
  where
    finalPm = sconcat pms

data Move
  = LetterDependent (PositionMappings -> LetterNameMappings)
  | LetterIndependent PositionMappings

emptyPositionMapping16 = Vec.fromList [(x, x) | x <- [0 .. 15]]

parseSpin :: Parser Move
parseSpin = do
  s <- char 's'
  n <- int
  pure $ LetterIndependent (spin emptyPositionMapping16 n)

parseExchange :: Parser Move
parseExchange = do
  x <- char 'x'
  n1 <- int
  slash <- char '/'
  n2 <- int
  pure $ LetterIndependent (exchange emptyPositionMapping16 (n1, n2))

parseLetter :: Parser Letter
parseLetter = do
  (char 'a' >> pure A) <|> (char 'b' >> pure B) <|> (char 'c' >> pure C) <|>
    (char 'd' >> pure D) <|>
    (char 'e' >> pure E) <|>
    (char 'f' >> pure F) <|>
    (char 'g' >> pure G) <|>
    (char 'h' >> pure H) <|>
    (char 'i' >> pure I) <|>
    (char 'j' >> pure J) <|>
    (char 'k' >> pure K) <|>
    (char 'l' >> pure L) <|>
    (char 'm' >> pure M) <|>
    (char 'n' >> pure N) <|>
    (char 'o' >> pure O) <|>
    (char 'p' >> pure P)

parsePartner :: Parser Move
parsePartner = do
  p <- char 'p'
  l1 <- parseLetter
  slash <- char '/'
  l2 <- parseLetter
  pure $ LetterDependent ()
