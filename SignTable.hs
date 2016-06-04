module SignTable(SignTable,
                 Sign(..),
                 Interval(..), intervals,
                 selectIntervals, appendSignCol,
                 filterCols,
                 initTable) where

import Data.List as L
import Data.Maybe

import Polynomial

data SignTable = SignTable [Interval] [(Polynomial, [Sign])]
                 deriving (Eq, Ord, Show)

intervals (SignTable itvs _) = itvs

selectSigns :: Polynomial -> SignTable -> [Sign]
selectSigns p (SignTable _ pls) =
  snd $ fromJust $ L.find (\(pl, _) -> pl == p) pls

appendSignCol :: Polynomial -> Sign -> SignTable -> SignTable
appendSignCol p s (SignTable itvs pls) =
  SignTable itvs ((p, L.replicate (L.length $ itvs) s):pls)

data Sign = Zero | Pos | Neg deriving (Eq, Ord, Show)

data Value = Inf | NInf | Var String deriving (Eq, Ord, Show)

data Interval = Pt Value
              | Pair Value Value
                deriving (Eq, Ord, Show)

selectIntervals :: Sign -> Polynomial -> SignTable -> [Interval]
selectIntervals s p st =
  let pSigns = selectSigns p st in
   L.map fst $ L.filter (\(_, sgn) -> sgn == s) $ L.zip (intervals st) pSigns

filterCols :: (Polynomial -> Bool) -> SignTable -> SignTable
filterCols f (SignTable ivs polys) =
  SignTable ivs $ filter (\(p, _) -> f p) polys

initTable :: [(Polynomial, Sign)] -> SignTable
initTable ps = SignTable [Pair NInf Inf] $ map (\(p, s) -> (p, [s])) ps
