module Logic(Formula(..),
             projectFormula,
             con, dis, true, false,
             gtz, ltz, eqz) where

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

getPoly (Pr _ p) = p

projectFormula :: String -> Formula ArithPred -> Formula ArithPred
projectFormula var f =
  disjunction $ L.map fst $ L.filter (\(_, t) -> hasSatAssignment f t) $ signTables var $ L.map getPoly $ atomUnion f

hasSatAssignment :: Formula ArithPred -> SignTable -> Bool
hasSatAssignment f sts = (S.size $ satRows f sts) > 0

satRows :: Formula ArithPred -> SignTable -> Set Interval
satRows T sts = S.fromList $ intervals sts
satRows F _ = S.empty
satRows (Pred (Pr "=" p)) st = S.fromList $ selectIntervals Zero p st
satRows (Pred (Pr ">" p)) st = S.fromList $ selectIntervals Pos p st
satRows (Pred (Pr "<" p)) st = S.fromList $ selectIntervals Neg p st
satRows (Binop "\\/" l r) st = S.union (satRows l st) (satRows r st)
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

reconstructTable s p remMap (f, st) =
  let m = filterCols (\p -> M.member p remMap) st
      pSgnMap = pointSignMap p st
      m = condenseSignTable p remMap
      signMap = inferSigns p pSgnMap in
   error "reconstructSignTable"

baseSignTables :: [Polynomial] -> [(Formula ArithPred, SignTable)]
baseSignTables ps =
  L.filter (\(f, s) -> f /= F) $ L.map (\(f, s) -> (simplifyFm f, s)) $ L.foldr accumTables [] ps

simplifyFm (Unop s l) = tvSimp $ Unop s (simplifyFm l)
simplifyFm (Binop s l r) = tvSimp $ Binop s (simplifyFm l) (simplifyFm r)
simplifyFm pr@(Pred p) = simplifyPred pr

tvSimp (Binop "\\/" T _) = T
tvSimp (Binop "\\/" _ T) = T
tvSimp (Binop "/\\" T T) = T
tvSimp (Binop "/\\" F _) = F
tvSimp (Binop "/\\" _ F) = F
tvSimp f = f

simplifyPred f@(Pred (Pr "=" p)) = if p == zero then T else if isCon p then F else f
simplifyPred f@(Pred (Pr ">" p)) = if isCon p then
                                     if (getCon p) > 0 then T else F
                                   else f
simplifyPred f@(Pred (Pr "<" p)) = if isCon p then
                                     if (getCon p) < 0 then T else F
                                   else f
simplifyPred f = f

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
