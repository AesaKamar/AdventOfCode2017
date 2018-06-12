module Lib where

import           Control.Applicative
import           Data.Array
import           Data.Ix
import           Data.List           (inits, tails)
import           Data.Traversable    (sequenceA)

type Structure i a = ((Array i i), (Array i a))

arrLen = snd . bounds

spin :: [a] -> Int -> [a]
spin v offset = reverse $ take (length v) (drop offset (cycle $ reverse v))
-- exchange :: [a] -> (a, a) -> [a]
-- partner ::
