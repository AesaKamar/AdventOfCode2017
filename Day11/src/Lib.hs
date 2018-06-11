-- {-# LANGUAGE TemplateHaskell #-}
--
module Lib where

--
-- import Control.Lens
--
data Direction1
  = No
  | So
  | Ne
  | Sw
  | Nw
  | Se
  deriving (Show, Eq) --
-- data HexGrid = HexGrid
--   { _no :: [Direction]
--   , _so :: [Direction]
--   , _ne :: [Direction]
--   , _sw :: [Direction]
--   , _nw :: [Direction]
--   , _se :: [Direction]
--   } deriving (Show, Eq)
--
-- makeLenses ''HexGrid
--
-- move :: HexGrid -> Direction -> HexGrid
-- move grid dir =
--   case dir of
--     No -> prependTo no
--     So -> prependTo so
--     Ne -> prependTo ne
--     Sw -> prependTo sw
--     Nw -> prependTo nw
--     Se -> prependTo se
--   where
--     prependTo = \whichLens -> over whichLens (cons dir) grid
--
-- simplify :: HexGrid -> HexGrid
-- simplify grid =
--   let (simpNo, simpSo) = simplifyDirections (_no grid, _so grid)
--       (simpNw, simpSe) = simplifyDirections (_nw grid, _se grid)
--       (simpNe, simpSw) = simplifyDirections (_ne grid, _sw grid)
--   in HexGrid
--      { _no = simpNo
--      , _so = simpSo
--      , _ne = simpNe
--      , _sw = simpSw
--      , _nw = simpNw
--      , _se = simpSe
--      }
--
-- simplifyDirections :: ([Direction], [Direction]) -> ([Direction], [Direction])
-- simplifyDirections ([], []) = ([], [])
-- simplifyDirections (d1s, []) = (d1s, [])
-- simplifyDirections ([], d2s) = ([], d2s)
-- simplifyDirections (d1:d1s, d2:d2s) = simplifyDirections (d1s, d2s)
