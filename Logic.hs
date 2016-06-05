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
  disjunction $ L.map fst $ L.filter (\(_, t) -> hasSatAssignment f t) $ signTables var $ L.nub $ L.map getPoly $ atomUnion f

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
         rems = L.map (\q -> pseudoDivide var p q) $ pd:rest
         newPs = pd:rest
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

pointSignMaps :: Polynomial ->
                 Map Polynomial (Polynomial, Polynomial) ->
                 SignTable ->
                 Map (Formula ArithPred) SignTable
pointSignMaps p remMap st =
  let pts = NInf:((points st) ++ [Inf]) in
   L.foldr (updateTables p remMap st) (M.fromList $ [(true, emptySignTable [p])]) pts

updateTables p remMap st pt maps =
  case pt of
   -- NOTE: Replace this with actual computation of sign of p at infinity
   Inf -> signProds p pt [(true, Pos)] maps
   NInf -> signProds p pt [(true, Neg)] maps
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
  let pSgnMaps = pointSignMaps p remMap st
      m = condenseSignTable p remMap st in
   L.map (\(g, stm) -> (simplifyFm $ con f g, stm)) $
   M.toList $ M.map (\newSt -> rebuildSignTable p newSt m) pSgnMaps

rebuildSignTable :: Polynomial ->
                    SignTable ->
                    SignTable ->
                    SignTable
rebuildSignTable p ptSt oldSt =
  let oldIntervals = intervals oldSt in
   snd $ L.foldr (updateInterval p ptSt oldSt) (0, emptySignTable (p:(columnLabels oldSt))) oldIntervals

updateInterval :: Polynomial ->
                  SignTable ->
                  SignTable ->
                  Interval ->
                  (Int, SignTable) ->
                  (Int, SignTable)
updateInterval p ptSt oldSt i (n, newSt) =
  case i of
   (Pt v) -> (n, continueRow p (lookupSign p i ptSt) oldSt i newSt)
   (Pair l r) ->
     case (lookupSign p (Pt l) ptSt, lookupSign p (Pt r) ptSt) of
      (Pos, Pos) -> (n, continueRow p Pos oldSt i newSt)
      (Neg, Neg) -> (n, continueRow p Pos oldSt i newSt)
      (Neg, Pos) -> (n + 1, splitRow2 n p Neg Zero Pos ptSt oldSt i newSt)
      -- NOTE: ???
      (Zero, Zero) -> (n + 1, continueRow p Pos oldSt i newSt)
      (Pos, Zero) -> (n, continueRow p Pos oldSt i newSt)

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

 --error $ "continueRow, p = " ++ show p ++ ", i = " ++ show i

                                            --    signMaps = inferSigns p pSgnMaps m
   --    n = deleteColumn (derivative s p) m in
   -- L.map (\(g, m) -> (simplifyFm $ con f g, mergeMap p m n)) signMaps

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
