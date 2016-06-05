module SignTable(SignTable,
                 Sign(..), Value(..),
                 Interval(..), intervals, emptySignTable,
                 selectIntervals, appendSignCol, selectSigns,
                 points, insertRow, insertRowSgn, entriesWithSignAt,
                 filterCols, deleteColumn, mergeMap,
                 spanIntervals, pointIntervals,
                 initTable, lookupSign, selectRow,
                 columnLabels) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Polynomial

-- Change to Map Polynomial [Sign] ?
data SignTable = SignTable [Interval] (Map Polynomial [Sign])
                 deriving (Eq, Ord, Show)

ulookup e m = fromJust $ M.lookup e m

selectRow :: Interval -> SignTable -> [(Polynomial, Sign)]
selectRow i st@(SignTable _ pls) =
  let ind = fromJust $ L.findIndex (\int -> int == i) $ intervals st in
   L.foldr (\k r -> (k, (ulookup k pls) !! ind):r) [] $ columnLabels st

insertRowSgn i sgn st =
  let ps = columnLabels st in
   insertRow i (L.zip ps (L.replicate (L.length ps) sgn)) st

insertRow i r (SignTable itvs pls) =
  SignTable (itvs ++ [i]) $ L.foldr (\(p, s) m -> appendSign p s m) pls r

appendSign p s m =
  let sgs = fromJust $ M.lookup p m in
   M.insert p (sgs ++ [s]) m

entriesWithSignAt sgn i st =
  let ps = columnLabels st in
   L.foldr (\p rest -> if lookupSign p i st == sgn then p:rest else rest) [] ps

numIntervals st = L.length $ intervals st

lookupSign p i s =
  let sgns = selectSigns p s in
   case L.find (\(it, _) -> it == i) $ L.zip (intervals s) sgns of
    Just (_, sgn) -> sgn
    Nothing -> error $ "lookupSign fail, sign table is\n" ++ show s ++
               "\nInterval is " ++ show i

intervals (SignTable itvs _) = itvs

spanIntervals st = L.filter isSpan $ intervals st
pointIntervals st = L.filter isPoint $ intervals st

points st = L.map (\(Pt v) -> v) $ pointIntervals st

selectSigns :: Polynomial -> SignTable -> [Sign]
selectSigns p s@(SignTable _ pls) =
  case M.lookup p pls of
    Just sgns -> sgns
    Nothing -> error $ "selectSigns fail, sign table is\n" ++ show s ++
               "\np is " ++ show p

columnLabels (SignTable _ pls) = M.keys pls

appendSignCol :: Polynomial -> Sign -> SignTable -> SignTable
appendSignCol p s st =
  appendCol p (L.replicate (numIntervals st) s) st

appendCol :: Polynomial -> [Sign] -> SignTable -> SignTable
appendCol p sgs st@(SignTable itvs pls) =
  SignTable itvs (M.insert p sgs pls)

deleteColumn :: Polynomial -> SignTable -> SignTable
deleteColumn p (SignTable itvs pls) =
  SignTable itvs $ M.delete p pls

mergeMap p sgMap st =
  let signs = L.foldr (\i sgs -> (fromJust $ M.lookup i sgMap):sgs) [] (intervals st) in
   appendCol p signs st

data Sign = Zero | Pos | Neg deriving (Eq, Ord)

instance Show Sign where
  show Zero = "0"
  show Pos = "+"
  show Neg = "-"

data Value = Inf | NInf | Var String deriving (Eq, Ord)

instance Show Value where
  show Inf = "+inf"
  show NInf = "-inf"
  show (Var s) = s

data Interval = Pt Value
              | Pair Value Value
                deriving (Eq, Ord)

instance Show Interval where
  show (Pt v) = "(" ++ show v ++ ")"
  show (Pair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"

isSpan (Pair _ _) = True
isSpan _ = False

isPoint i = not $ isSpan i

selectIntervals :: Sign -> Polynomial -> SignTable -> [Interval]
selectIntervals s p st =
  let pSigns = selectSigns p st in
   L.map fst $ L.filter (\(_, sgn) -> sgn == s) $ L.zip (intervals st) pSigns

filterCols :: (Polynomial -> Bool) -> SignTable -> SignTable
filterCols f (SignTable ivs polys) =
  SignTable ivs $ M.fromList $ L.filter (\(p, _) -> f p) $ M.toList polys

initTable :: [(Polynomial, Sign)] -> SignTable
initTable ps = SignTable [Pair NInf Inf] $ M.fromList $ L.map (\(p, s) -> (p, [s])) ps

emptySignTable :: [Polynomial] -> SignTable
emptySignTable ps = SignTable [] $ M.fromList $ L.map (\p -> (p, [])) ps
