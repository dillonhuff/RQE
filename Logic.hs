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
import PrettyPrint
import SignTable

data Formula a = Pred a
               | Binop String [Formula a]
               | Unop String (Formula a)
               | T
               | F
               | Quantifier String String (Formula a)
                 deriving (Eq, Ord)

instance Show a => Show (Formula a) where
  show (Pred p) = "(" ++ show p ++ ")"
  show (Binop s args) = "(" ++ sumList s args ++ ")"
  show T = "T"
  show F = "F"

true = T

false = F

con (Binop "/\\" l) (Binop "/\\" r) = Binop "/\\" (l ++ r)
con s@(Unop _ _) (Binop "/\\" r) = Binop "/\\" (s:r)
con s@(Pred _) (Binop "/\\" r) = Binop "/\\" (s:r)
con (Binop "/\\" r) s@(Unop _ _) = Binop "/\\" (s:r)
con (Binop "/\\" r) s@(Pred _) = Binop "/\\" (s:r)

con a b = Binop "/\\" [a, b]

dis a b = Binop "\\/" [a, b]

conjunction [] = true
conjunction r = Binop "/\\" r

disjunction [] = false
disjunction r = Binop "\\/" r

atomUnion (Pred a) = [a]
atomUnion (Binop _ [l, r]) = atomUnion l ++ atomUnion r
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
applyDown f (Binop s args) = f $ Binop s $ L.map (applyDown f) args
applyDown f pr@(Pred p) = f pr
applyDown f T = T
applyDown f F = F

simplifyFm f = applyDown (tvSimp . simplifyPred) f

tvSimp :: (Eq a) => Formula a -> Formula a
tvSimp (Binop "\\/" [T, _]) = T
tvSimp (Binop "\\/" [_, T]) = T
tvSimp (Binop "/\\" [T, T]) = T
tvSimp (Binop "/\\" [F, _]) = F
tvSimp (Binop "/\\" [_, F]) = F
tvSimp (Binop "/\\" [T, l]) = l
tvSimp (Binop "/\\" [r, T]) = r
tvSimp b@(Binop "\\/" a) =
  let args = L.filter (\f -> f /= F) a in
   if any (\f -> f == T) args then T else disjunction args
tvSimp b@(Binop "/\\" a) =
  let args = L.filter (\f -> f /= T) a in
   if all (\f -> f == T) args then T else
     if any (\f -> f == F) args then F else conjunction args
tvSimp f = f

simplifyPred f@(Pred (Pr "=" p)) = if p == zero then T else if isCon p then F else f
simplifyPred f@(Pred (Pr ">" p)) = if isCon p then
                                     if (getCon p) > 0 then T else F
                                   else if (isPos p) then T else f
simplifyPred f@(Pred (Pr "<" p)) = if isCon p then
                                     if (getCon p) < 0 then T else F
                                   else if (isPos p) then F else f
simplifyPred f = f

getPoly (Pr _ p) = p

isPred (Pred _) = True
isPred _ = False

getPred (Pred p) = p
getPred _ = error "getPred"

isEqZero (Pr "=" _) = True
isEqZero _ = False

simpClause :: [ArithPred] -> [ArithPred]
simpClause a =
  let monicized = L.map (\(Pr s p) -> Pr s (tryMonicize p)) a
      zeros = L.filter (\c -> isEqZero c) monicized
      nonZeros = L.filter (\c -> not $ isEqZero c) monicized in
   L.foldr (\(Pr s p) cs -> if any (\(Pr _ q) -> divEven p q /= Nothing) zeros then (Pr s zero):cs else (Pr s p):cs) nonZeros zeros

simplifyClause (Binop "/\\" args) =
  let smArgs = L.map simplifyFm args in
   if all isPred smArgs
      then
     let sArgs = simpClause $ L.map getPred smArgs in
      if isContradiction sArgs
         then F
      else conjunction $ L.map Pred sArgs
   else conjunction smArgs
simplifyClause f = f

isContra (Pr s p) (Pr l q) = p == q && s /= l

isContradiction :: [ArithPred] -> Bool
isContradiction a =
  L.or [isContra x y | x <- a, y <- a]

projectFormula :: String -> Formula ArithPred -> Formula ArithPred
projectFormula var f =
  simplifyFm $ disjunction $
  L.map simplifyClause $ L.map fst $
  L.filter (\(_, t) -> hasSatAssignment f t) $
  signTables var $ L.nub $ L.map getPoly $ atomUnion f

hasSatAssignment :: Formula ArithPred -> SignTable -> Bool
hasSatAssignment f sts = (S.size $ satRows f sts) > 0

satRows :: Formula ArithPred -> SignTable -> Set Interval
satRows T sts = S.fromList $ intervals sts
satRows F _ = S.empty
satRows (Pred (Pr "=" p)) st = S.fromList $ selectIntervals Zero p st
satRows (Pred (Pr ">" p)) st = S.fromList $ selectIntervals Pos p st
satRows (Pred (Pr "<" p)) st = S.fromList $ selectIntervals Neg p st
satRows (Binop "\\/" args) st = S.unions $ L.map (\f -> satRows f st) args
satRows (Binop "/\\" args) st = L.foldr (\s k -> S.intersection s k) (S.fromList $ intervals st) $ L.map (\f -> satRows f st) args
satRows f sts = error $ "satRows, f = " ++ show f

signTables :: String -> [Polynomial] -> [(Formula ArithPred, SignTable)]
signTables var ps =
  case all (\p -> deg var p == 0) ps of
   True -> baseSignTables ps
   False ->
     let p = fromJust $ L.find (\p -> deg var p > 0) ps
         pd = derivative var p
         rest = L.filter (\q -> q /= p) ps
         newPs = pd:rest
         rems = L.map (\q -> pseudoDivide var p q) newPs
         remMap = M.fromList $ L.zip newPs $ L.map (\(b, _, r) -> (b, r)) rems
         nextPolys = newPs ++ (L.map (\(_, _, r) -> r) rems)
         sts = signTables var nextPolys in
      reconstructTables var p remMap sts

reconstructTables :: String ->
                     Polynomial ->
                     Map Polynomial (Polynomial, Polynomial) ->
                     [(Formula ArithPred, SignTable)] ->
                     [(Formula ArithPred, SignTable)]
reconstructTables s p remMap sts =
  L.concatMap (reconstructTable s p remMap) sts

pointSignMaps :: String ->
                 Polynomial ->
                 Map Polynomial (Polynomial, Polynomial) ->
                 SignTable ->
                 Map (Formula ArithPred) SignTable
pointSignMaps s p remMap st =
  let pts = NInf:((points st) ++ [Inf]) in
   L.foldr (updateTables s p remMap st) (M.fromList $ [(true, emptySignTable [p])]) pts

signAtInfD var p pd st =
  case (signAtInf pd st, even $ deg var p) of
   (Pos, _) -> [(true, Pos)]
   (Neg, _) -> [(true, Neg)]
   (Zero, _) ->
     let b = deleteLcof var p in
      [(gtz b, Pos), (ltz b, Neg), (eqz b, Zero)]

signAtNInfD var p pd st =
  case (signAtNInf pd st, even $ deg var p) of
   (Pos, True) -> [(true, Pos)]
   (Pos, False) -> [(true, Neg)]
   (Neg, _) -> [(true, Neg)]
   (Zero, _) ->
     let b = deleteLcof var p in
      [(gtz b, Pos), (ltz b, Neg), (eqz b, Zero)]

updateTables s p remMap st pt maps =
  let pd = derivative s p in
   case pt of
    Inf -> signProds p pt (signAtInfD s p pd st) maps
    NInf -> signProds p pt (signAtNInfD s p pd st) maps
    _ ->
      let (b, r) = findPseudoRem remMap st pt
          fmSigns = caseSplitSigns b $ lookupSign r (Pt pt) st in
       signProds p pt fmSigns maps

caseSplitSigns :: Polynomial -> Sign -> [(Formula ArithPred, Sign)]
caseSplitSigns b sgn =
  case sgn of
   Zero -> [(gtz b, Zero), (ltz b, Zero), (eqz b, Pos), (eqz b, Neg), (eqz b, Zero)]
   Pos -> [(gtz b, Pos), (ltz b, Neg)]
   Neg -> [(gtz b, Neg), (ltz b, Pos)]

signProds :: Polynomial ->
             Value ->
             [(Formula ArithPred, Sign)] ->
             Map (Formula ArithPred) SignTable ->
             Map (Formula ArithPred) SignTable
signProds p pt sgns maps =
  M.unions $ L.map (\(f, sgn) -> M.mapKeys (\k -> simplifyFm $ con f k) $
                                 M.map (\st -> insertRowSgn (Pt pt) sgn st) maps) sgns

findPseudoRem remMap st pt =
  let p = L.head $ L.filter (\p -> M.member p remMap) $ entriesWithSignAt Zero (Pt pt) st in
   fromJust $ M.lookup p remMap

condenseSignTable p remMap st =
  filterCols (\p -> not $ elem p $ L.map (\(_, (_, r)) -> r) $ M.toList remMap) st

reconstructTable s p remMap (f, st) =
  let pSgnMaps = pointSignMaps s p remMap st
      m = condenseSignTable p remMap st in
   L.map (\(g, stm) -> (simplifyFm $ con f g, stm)) $
   M.toList $ M.map (\newSt -> rebuildSignTable s p newSt m) pSgnMaps

rebuildSignTable :: String ->
                    Polynomial ->
                    SignTable ->
                    SignTable ->
                    SignTable
rebuildSignTable s p ptSt oldSt =
  let oldIntervals = intervals oldSt in
   renumberIntervals $ deleteColumn (derivative s p) $ L.foldr (updateInterval p ptSt oldSt) (emptySignTable (p:(columnLabels oldSt))) oldIntervals

renumberIntervals :: SignTable -> SignTable
renumberIntervals st = setIntervals (rrn 0 $ intervals st) st

rrn _ [] = []
rrn n ((Pair NInf _):rest) = (Pair NInf (Var $ "x" ++ show n)):(rrn n rest)
rrn n ((Pair _ Inf):[]) = [Pair (Var $ "x" ++ show n) Inf]
rrn n ((Pair (Var _) (Var _)):rest) = (Pair (Var $ "x" ++ show n) (Var $ "x" ++ (show (n+1)))):(rrn (n+1) rest)
rrn n ((Pt _):rest) = (Pt $ Var $ "x" ++ show n ):(rrn n rest)
rrn _ x = error $ "rrn: " ++ show x
   
updateInterval :: Polynomial ->
                  SignTable ->
                  SignTable ->
                  Interval ->
                  SignTable ->
                  SignTable
updateInterval p ptSt oldSt i newSt =
  case i of
   (Pt v) -> continueRow p (lookupSign p i ptSt) oldSt i newSt
   (Pair l r) ->
     case (lookupSign p (Pt l) ptSt, lookupSign p (Pt r) ptSt) of
      (Pos, Pos) -> continueRow p Pos oldSt i newSt
      (Neg, Neg) -> continueRow p Pos oldSt i newSt
      (Neg, Pos) -> splitRow2 0 p Neg Zero Pos ptSt oldSt i newSt
      (Pos, Neg) -> splitRow2 0 p Pos Zero Neg ptSt oldSt i newSt
      -- NOTE: ???
      (Zero, Zero) -> continueRow p Pos oldSt i newSt
      (Pos, Zero) -> continueRow p Pos oldSt i newSt
      (Zero, Pos) -> continueRow p Pos oldSt i newSt
      (Neg, Zero) -> continueRow p Neg oldSt i newSt
      (Zero, Neg) -> continueRow p Neg oldSt i newSt

continueRow p s oldSt i newSt =
  let contRow = (p, s):(selectRow i oldSt) in
   insertRow i contRow newSt

splitRow2 n p sl sz sr ptSt oldSt i@(Pair l r) newSt =
  let rw = selectRow i oldSt
      r1 = (p, sl):rw
      r2 = (p, sz):rw
      r3 = (p, sr):rw
      i1 = Pair l (Var ("x" ++ (show n)))
      i2 = Pt (Var ("x" ++ (show n)))
      i3 = Pair (Var ("x" ++ (show n))) r in
   insertRow i3 r3 $ insertRow i2 r2 $ insertRow i1 r1 newSt

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
