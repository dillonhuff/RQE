module SignTable(SignTable,
                 Sign(..), Value(..),
                 Interval(..), intervals, emptySignTable, setIntervals,
                 selectIntervals, appendSignCol, selectSigns,
                 points, insertRow, insertRowSgn, entriesWithSignAt,
                 filterCols, deleteColumn, mergeMap,
                 spanIntervals, pointIntervals, setSign, splitRow,
                 initTable, lookupSign, selectRow, renumberIntervals,
                 signAtInf, signAtNInf, filterRedundantIntervals,
                 columnLabels) where

import Data.List as L
import Data.Map as M
import Data.Maybe

import Polynomial

-- Change to Map Polynomial [Sign] ?
data SignTable = SignTable [Interval] (Map Polynomial [Sign])
                 deriving (Eq, Ord, Show)

ulookup e m = fromJust $ M.lookup e m

setIntervals i (SignTable _ pls) = SignTable i pls

setSign :: Polynomial -> Interval -> Sign -> SignTable -> SignTable
setSign p i s st =
  let pSgns = selectSigns p st
      ind = fromJust $ L.findIndex (\it -> it == i) $ intervals st
      prefix = take ind pSgns
      suffix = drop (ind+1) pSgns in
   appendCol p (prefix ++ [s] ++ suffix) st

filterRedundantIntervals st =
  let pts = pointIntervals st in
   clipRedundant pts st --error "e" --

clipRedundant :: [Interval] -> SignTable -> SignTable
clipRedundant pts st = L.foldr clipR st pts

clipR i st =
  let ind = fromJust $ L.findIndex (\it -> i == it) $ intervals st
      l = (intervals st) !! (ind - 1)
      r = (intervals st) !! (ind + 1) in
   if L.all (\p -> sameOn p [l, i, r] st) $ columnLabels st
   then error "clipR"
   else st
      
sameOn p inds st =
  L.length (L.nub $ L.map (\i -> lookupSign p i st) inds) == 1

renumberIntervals :: SignTable -> SignTable
renumberIntervals st = setIntervals (rrn 0 $ intervals st) st

rrn _ [] = []
rrn n ((Pair NInf Inf):[]) = [Pair NInf Inf]
rrn n ((Pair NInf _):rest) = (Pair NInf (Var $ "x" ++ show n)):(rrn n rest)
rrn n ((Pair _ Inf):[]) = [Pair (Var $ "x" ++ show n) Inf]
rrn n ((Pair (Var _) (Var _)):rest) = (Pair (Var $ "x" ++ show n) (Var $ "x" ++ (show (n+1)))):(rrn (n+1) rest)
rrn n ((Pt _):rest) = (Pt $ Var $ "x" ++ show n ):(rrn n rest)
rrn _ x = error $ "rrn: " ++ show x

dummyTriple (Pair l r) =
  (Pair l (Var "tmp"), Pt (Var "tmp"), Pair (Var "tmp") r)

newPls p (l, c, r) i pls =
  M.fromList $ L.map (insertTripleOrDup p (l, c, r) i) $ M.toList pls

insertTripleOrDup p (l, c, r) i (q, sgns) =
  if p == q
  then (q, insertTriple (l, c, r) i sgns)
  else (q, duplicateTripleAt i sgns)

duplicateTripleAt i sgns =
  let prefix = take i sgns
      suffix = drop (i+1) sgns
      v = sgns !! i in
   prefix ++ [v, v, v] ++ suffix
  
insertTriple (l, c, r) i intervals =
  let prefix = take i intervals
      suffix = drop (i+1) intervals in
   prefix ++ [l, c, r] ++ suffix

splitRow :: Interval -> Polynomial -> (Sign, Sign, Sign) -> SignTable -> SignTable
splitRow i p signTriple st@(SignTable itvs pls) =
  let ind = fromJust $ L.findIndex (\it -> it == i) $ intervals st
      newIntTriple = dummyTriple i
      nPls = newPls p signTriple ind pls
      nIntervals = insertTriple newIntTriple ind itvs in
   SignTable nIntervals nPls

selectRow :: Interval -> SignTable -> [(Polynomial, Sign)]
selectRow i st@(SignTable _ pls) =
  let ind = fromJust $ L.findIndex (\int -> int == i) $ intervals st in
   L.foldr (\k r -> (k, (ulookup k pls) !! ind):r) [] $ columnLabels st

insertRowSgn i sgn st =
  let ps = columnLabels st in
   insertRow i (L.zip ps (L.replicate (L.length ps) sgn)) st

insertRow i r (SignTable itvs pls) =
  SignTable (itvs ++ [i]) $ L.foldr (\(p, s) m -> appendSign p s m) pls r

signAtNInf p st =
  lookupSign p (L.head $ intervals st) st

signAtInf p st =
  lookupSign p (L.head $ L.reverse $ intervals st) st

appendSign p s m =
  let sgs = fromJust $ M.lookup p m in
   M.insert p (sgs ++ [s]) m

entriesWithSignAt sgn i st =
  let ps = columnLabels st in
   L.foldr (\p rest -> if lookupSign p i st == sgn then p:rest else rest) [] ps

numIntervals st = L.length $ intervals st

conSign i = if i == 0 then Zero else if i > 0 then Pos else Neg

lookupSign p i s =
  if isCon p then conSign $ getCon p else
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
