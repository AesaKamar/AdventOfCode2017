module Lib2 where

import           Data.Monoid

data HexMove =
  HexMove Integer
          Integer
          Integer
  deriving (Eq, Show)

data Direction
  = No
  | So
  | Ne
  | Sw
  | Nw
  | Se
  deriving (Show, Eq)

parse :: String -> Direction
parse "n"  = No
parse "s"  = So
parse "ne" = Ne
parse "sw" = Sw
parse "nw" = Nw
parse "se" = Se

move :: Direction -> HexMove
move dir =
  case dir of
    No -> HexMove 0 1 (-1)
    So -> HexMove 0 (-1) 1
    Ne -> HexMove 1 0 (-1)
    Sw -> HexMove (-1) 0 (1)
    Nw -> HexMove (-1) (1) (0)
    Se -> HexMove (0) (-1) (1)

instance Monoid HexMove where
  mempty = HexMove 0 0 0
  mappend (HexMove a1 b1 c1) (HexMove a2 b2 c2) =
    HexMove (a1 + a2) (b1 + b2) (c1 + c2)

distance :: HexMove -> HexMove -> Integer
distance (HexMove a1 b1 c1) (HexMove a2 b2 c2) =
  max (max (a1 - a2) (b1 - b2)) (c1 - c2)
