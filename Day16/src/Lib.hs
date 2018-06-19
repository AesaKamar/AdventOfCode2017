{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Control.Applicative
import           Data.Array
import           Data.Char           (toLower)
import           Data.Ix
import           Data.List           (elemIndex, inits, tails)
import           Data.Maybe          (fromJust)
import           Data.Traversable    (sequenceA)

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

data Structure = Structure
  { cursor  :: Int
  , len     :: Int
  , letters :: Array Int Letter
  }

instance Show Structure where
  show s = toLower <$> (concat $ show <$> theLettersStartingAtCursor)
    where
      theLetters = elems $ letters s
      len = length theLetters
      modPos = cursor s `mod` len
      theLettersStartingAtCursor = take len (drop modPos (cycle theLetters))

initialStructure =
  Structure
  { cursor = 0
  , len = 16
  -- , indicies = listArray (0, 15) [0 .. 15]
  , letters = listArray (0, 15) [A .. P]
  }

arrLen = snd . bounds

spin :: Structure -> Int -> Structure
spin s offset =
  Structure
  {cursor = (cursor s - offset) `mod` (len s), letters = letters s, len = len s}

exchange :: Structure -> (Int, Int) -> Structure
exchange s (p1, p2) =
  Structure {cursor = cursor s, len = len s, letters = newLtrs}
  where
    ltrs = letters s
    newLtrs = ltrs // [(p1, ltrs ! p2), (p2, ltrs ! p1)]

partner :: Structure -> (Letter, Letter) -> Structure
partner s (l1, l2) =
  let elements = elems (letters s)
      p1 = elemIndex l1 elements
      p2 = elemIndex l2 elements
  in exchange s (fromJust p1, fromJust p2)
