module Logic(Formula(..),
             projectFormula,
             con, dis, true, false,
             gtz, ltz, eqz, simplifyFm,
             signTables) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

import Polynomial
import SignTable

data Formula a = Pred a
               | Binop String (Formula a) (Formula a)
               | Unop String (Formula a)
               | T
               | F
               | Quantifier String String (Formula a)
                 deriving (Eq, Ord)

instance Show a => Show (Formula a) where
  show (Pred p) = "(" ++ show p ++ ")"
  show (Binop s l r) = "(" ++ show l ++ " " ++ s ++ " " ++ show r ++ ")"
  show T = "T"
  show F = "F"

true = T
false = F
con = Binop "/\\"
dis = Binop "\\/"

disjunction [] = false
disjunction x = L.foldr (\f g -> dis f g) (L.head x) (L.tail x)

atomUnion (Pred a) = [a]
atomUnion (Binop _ l r) = atomUnion l ++ atomUnion r
atomUnion (Unop _ l) = atomUnion l
atomUnion (Quantifier _ _ f) = atomUnion f
atomUnion _ = []

data ArithPred = Pr String Polynomial
                 deriving (Eq, Ord)

instance Show ArithPred where
  show (Pr s p) = show p ++ " " ++ s ++ " 0"
gtz p = Pred $ Pr ">" p
ltz p = Pred $ Pr "<" p
eqz p = Pred $ Pr "=" p


applyDown f (Unop s l) = f $ Unop s (applyDown f l)
applyDown f (Binop s l r) = f $ Binop s (applyDown f l) (applyDown f r)
applyDown f pr@(Pred p) = f pr
applyDown f T = T
applyDown f F = F


simplifyFm f = applyDown (tvSimp . simplifyPred) f

tvSimp (Binop "\\/" T _) = T
tvSimp (Binop "\\/" _ T) = T
tvSimp (Binop "/\\" T T) = T
tvSimp (Binop "/\\" F _) = F
tvSimp (Binop "/\\" _ F) = F
tvSimp (Binop "/\\" T l) = l
tvSimp (Binop "/\\" r T) = r
tvSimp f = f

simplifyPred f@(Pred (Pr "=" p)) = if p == zero then T else if isCon p then F else f
simplifyPred f@(Pred (Pr ">" p)) = if isCon p then
                                     if (getCon p) > 0 then T else F
                                   else f
simplifyPred f@(Pred (Pr "<" p)) = if isCon p then
                                     if (getCon p) < 0 then T else F
                                   else f
simplifyPred f = f

getPoly (Pr _ p) = p

projectFormula :: String -> Formula ArithPred -> Formula ArithPred
projectFormula var f =
  disjunction $ L.map fst $ L.filter (\(_, t) -> hasSatAssignment f t) $ signTables var $ L.map getPoly $ L.nub $ atomUnion f

hasSatAssignment :: Formula ArithPred -> SignTable -> Bool
hasSatAssignment f sts = (S.size $ satRows f sts) > 0

satRows :: Formula ArithPred -> SignTable -> Set Interval
satRows T sts = S.fromList $ intervals sts
satRows F _ = S.empty
satRows (Pred (Pr "=" p)) st = S.fromList $ selectIntervals Zero p st
satRows (Pred (Pr ">" p)) st = S.fromList $ selectIntervals Pos p st
satRows (Pred (Pr "<" p)) st = S.fromList $ selectIntervals Neg p st
satRows (Binop "\\/" l r) st = S.union (satRows l st) (satRows r st)
satRows (Binop "/\\" l r) st = S.intersection (satRows l st) (satRows r st)
satRows f sts = error $ "satRows, f = " ++ show f

signTables :: String -> [Polynomial] -> [(Formula ArithPred, SignTable)]
signTables var ps =
  case all (\p -> deg var p == 0) ps of
   True -> baseSignTables ps
   False ->
     let p = fromJust $ L.find (\p -> deg var p > 0) ps
         pd = derivative var p
         rest = L.filter (\q -> q /= p) ps
         rems = L.map (\q -> pseudoRemainder var p q) $ pd:rest
         newPs = pd:rest
         remMap = M.fromList $ L.zip newPs rems
         nextPolys = newPs ++ rems
         sts = signTables var nextPolys in
      reconstructTables var p remMap sts

reconstructTables :: String ->
                     Polynomial ->
                     Map Polynomial Polynomial ->
                     [(Formula ArithPred, SignTable)] ->
                     [(Formula ArithPred, SignTable)]
reconstructTables s p remMap sts =
  L.concatMap (reconstructTable s p remMap) sts

pointSignMap p remMap st =
  L.foldr (\i m -> M.insert i (findRemSign remMap st i) m) M.empty (pointIntervals st)

findRemSign remMap st i = error "findRemSign"

condenseSignTable p remMap st =
  filterCols (\p -> not $ elem p $ L.map snd $ M.toList remMap) st

inferSigns :: Polynomial ->
              Map Interval Sign ->
              SignTable ->
              [(Formula ArithPred, Map Interval Sign)]
inferSigns p pSignMap st =
  let ints = spanIntervals st in
   L.foldr (signMaps p st) [(true, pSignMap)] ints

signMaps :: Polynomial ->
            SignTable ->
            Interval ->
            [(Formula ArithPred, Map Interval Sign)] ->
            [(Formula ArithPred, Map Interval Sign)]
signMaps p st i maps =
  let fmValPairs = signs p st i in
   [(con f g, M.insert i v m) | (f, m) <- maps, (g, v) <- fmValPairs]

signs :: Polynomial -> SignTable -> Interval -> [(Formula ArithPred, Sign)]
signs p s i = error "signs"
  -- case i of
  --  Pt v -> 
  --    let sgn = lookupSign p i s in
  --     [(true, sgn)]
  --  Pair l r ->
  --    case (signAt p s l, signAt p s r) of
  --     (Pos, Pos) -> [(true, Pos)]
  --     (Neg, Pos) -> [(true, Pos)]
  --     (Pos, Pos) -> [(true, Pos)]

reconstructTable s p remMap (f, st) =
  let pSgnMap = pointSignMap p remMap st
      m = condenseSignTable p remMap st
      signMaps = inferSigns p pSgnMap m
      n = deleteColumn (derivative s p) m in
   L.map (\(g, m) -> (simplifyFm $ con f g, mergeMap p m n)) signMaps

baseSignTables :: [Polynomial] -> [(Formula ArithPred, SignTable)]
baseSignTables ps =
  L.filter (\(f, s) -> f /= F) $ L.map (\(f, s) -> (simplifyFm f, s)) $ L.foldr accumTables [] ps

accumTables :: Polynomial ->
               [(Formula ArithPred, SignTable)] ->
               [(Formula ArithPred, SignTable)]
accumTables p [] = baseTables p
accumTables p tbls =
  (L.map (\t -> appendAll (gtz p) p Pos t) tbls) ++
  (L.map (\t -> appendAll (ltz p) p Neg t) tbls) ++
  (L.map (\t -> appendAll (eqz p) p Zero t) tbls)

appendAll :: Formula ArithPred ->
             Polynomial ->
             Sign ->
             (Formula ArithPred, SignTable) ->
             (Formula ArithPred, SignTable)
appendAll pr p s (f, st) =
  (con pr f, appendSignCol p s st)

baseTables p = [(gtz p, initTable [(p, Pos)]),
                (ltz p, initTable [(p, Neg)]),
                (eqz p, initTable [(p, Zero)])]
