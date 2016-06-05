module SignTable(SignTable,
                 Sign(..), Value(..),
                 Interval(..), intervals,
                 selectIntervals, appendSignCol, selectSigns,
                 filterCols, deleteColumn, mergeMap,
                 spanIntervals, pointIntervals,
                 initTable, lookupSign,
                 columnLabels) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Polynomial

data SignTable = SignTable [Interval] [(Polynomial, [Sign])]
                 deriving (Eq, Ord, Show)

lookupSign p i s =
  let sgns = selectSigns p s in
   case L.find (\(it, _) -> it == i) $ L.zip (intervals s) sgns of
    Just (_, sgn) -> sgn
    Nothing -> error $ "lookupSign fail, sign table is\n" ++ show s

intervals (SignTable itvs _) = itvs

spanIntervals st = L.filter isSpan $ intervals st
pointIntervals st = L.filter isPoint $ intervals st

selectSigns :: Polynomial -> SignTable -> [Sign]
selectSigns p s@(SignTable _ pls) =
  case L.find (\(pl, _) -> pl == p) pls of
    Just (_, sgns) -> sgns
    Nothing -> error $ "selectSigns fail, sign table is\n" ++ show s ++
               "\np is " ++ show p

columnLabels (SignTable _ pls) = L.map fst pls

appendSignCol :: Polynomial -> Sign -> SignTable -> SignTable
appendSignCol p s (SignTable itvs pls) =
  SignTable itvs ((p, L.replicate (L.length $ itvs) s):pls)

appendCol :: Polynomial -> [Sign] -> SignTable -> SignTable
appendCol p sgs (SignTable itvs pls) =
  SignTable itvs ((p, sgs):pls)

deleteColumn :: Polynomial -> SignTable -> SignTable
deleteColumn p (SignTable itvs pls) =
  SignTable itvs $ L.filter (\(q, _) -> q /= p) pls

mergeMap p sgMap st =
  let signs = L.foldr (\i sgs -> (fromJust $ M.lookup i sgMap):sgs) [] (intervals st) in
   appendCol p signs st

data Sign = Zero | Pos | Neg deriving (Eq, Ord, Show)

data Value = Inf | NInf | Var String deriving (Eq, Ord, Show)

data Interval = Pt Value
              | Pair Value Value
                deriving (Eq, Ord, Show)

isSpan (Pair _ _) = True
isSpan _ = False

isPoint i = not $ isSpan i

selectIntervals :: Sign -> Polynomial -> SignTable -> [Interval]
selectIntervals s p st =
  let pSigns = selectSigns p st in
   L.map fst $ L.filter (\(_, sgn) -> sgn == s) $ L.zip (intervals st) pSigns

filterCols :: (Polynomial -> Bool) -> SignTable -> SignTable
filterCols f (SignTable ivs polys) =
  SignTable ivs $ L.filter (\(p, _) -> f p) polys

initTable :: [(Polynomial, Sign)] -> SignTable
initTable ps = SignTable [Pair NInf Inf] $ L.map (\(p, s) -> (p, [s])) ps
