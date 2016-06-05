module Polynomial(Polynomial,
                  mkCon, mkPoly, mkMono, one, zero,
                  isCon, getCon,
                  deg, tryMonicize,
                  lcof, isPos,
                  deleteLcof,
                  plus, times, divEven,
                  derivative, divideEvenly,
                  divide, pseudoDivide, pseudoRemainder) where

import Data.List as L
import Data.Map as M
import Data.Set as S

import PrettyPrint

data Monomial = Monomial Integer (Map String Integer)
                deriving (Eq, Ord)

mkMono :: Integer -> [(String, Integer)] -> Monomial
mkMono i vars = Monomial i (M.fromList $ L.filter (\(_, c) -> c /= 0) vars)

mkMonoMap i vars = Monomial i (M.filter (\c -> c /= 0) vars)

monoCoeff (Monomial c _) = c

evenPowers (Monomial _ m) = L.all (\(_, i) -> even i) $ M.toList m

vars :: Monomial -> Set String
vars (Monomial _ vs) = M.keysSet vs

monomialTimes :: Integer -> Monomial -> Monomial
monomialTimes i (Monomial c ms) = mkMonoMap (i*c) ms

monomialProd :: Monomial -> Monomial -> Monomial
monomialProd (Monomial c1 m1) (Monomial c2 m2) =
  mkMonoMap (c1*c2) $ M.unionWith (\i j -> i + j) m1 m2

deleteVar :: String -> Monomial -> Monomial
deleteVar x (Monomial c vars) = mkMonoMap c $ M.delete x vars

setMonoDegree var d (Monomial c vars) =
  mkMonoMap c $ M.insert var d vars

monoDegree var (Monomial _ vars) =
  case M.lookup var vars of
   Just d -> d
   Nothing -> 0

instance Show Monomial where
  show (Monomial c vars) =
    let varList = M.toList vars in
     if c /= 1 && (length varList) /= 0
     then show c ++ "*" ++ (printVars $ M.toList vars)
     else if length varList == 0
          then show c
          else (printVars $ M.toList vars)

printVars :: [(String, Integer)] -> String
printVars [] = ""
printVars ((x, p):[]) = showExp x p
printVars ((x, p):(y, q):rest) =
  (showExp x p) ++ "*" ++
  (showExp y q) ++ (printVars rest)

showExp x 1 = x
showExp x p = x ++ "^" ++ show p

data Polynomial = Polynomial (Set Monomial)
                  deriving (Eq, Ord)

instance Show Polynomial where
  show (Polynomial rs) =
    let mList = S.toList rs in
     if length mList == 0
     then "0"
     else sumList "+" $ S.toList rs

isCon p = (S.size $ varSet p) == 0

one = mkPoly [mkMono 1 []]
zero = mkPoly [mkMono 0 []]

mkPoly :: [Monomial] -> Polynomial
mkPoly monomials = Polynomial $ S.fromList $ L.filter (\m -> monoCoeff m /= 0) monomials

mkCon :: Integer -> Polynomial
mkCon i = mkPoly [mkMono i []]

mkPolyS :: Set Monomial -> Polynomial
mkPolyS monomials = Polynomial $ S.filter (\m -> monoCoeff m /= 0) monomials

deleteLcof var p@(Polynomial ms) =
  Polynomial $ S.filter (\m -> monoDegree var m < deg var p) ms

timesInt :: Integer -> Polynomial -> Polynomial
timesInt i (Polynomial ms) = Polynomial (S.map (\m -> monomialTimes i m) ms)

times :: Polynomial -> Polynomial -> Polynomial
times (Polynomial ps) (Polynomial qs) =
  let p = S.toList ps
      q = S.toList qs in
   mkPoly $ simplify $ L.concatMap (\m -> L.map (\t -> monomialProd m t) q) p

plus :: Polynomial -> Polynomial -> Polynomial
plus (Polynomial m1) (Polynomial m2) = mkPoly $ simplify ((S.toList m1) ++ (S.toList m2))

minus :: Polynomial -> Polynomial -> Polynomial
minus f g = plus f (timesInt (-1) g)

simplify :: [Monomial] -> [Monomial]
simplify ms =
  let sorted = sortBy (\(Monomial _ m1) (Monomial _ m2) -> compare m1 m2) ms
      grouped = groupBy (\(Monomial _ m1) (Monomial _ m2) -> m1 == m2) sorted
      mPlus = (\(Monomial c1 m1) (Monomial c2 _) -> Monomial (c1 + c2) m1)
      results = L.map (\terms -> L.foldr mPlus (L.head terms) (L.tail terms)) grouped in
   results

pow :: Polynomial -> Integer -> Polynomial
pow p 0 = one
pow p 1 = p
pow p n = pow (times p p) (n-1)

lcof :: String -> Polynomial -> Polynomial
lcof var p@(Polynomial ts) = Polynomial $ S.map (deleteVar var) $ S.filter (\m -> (monoDegree var m) == (deg var p)) ts

deg :: String -> Polynomial -> Integer
deg var (Polynomial ts) =
  if length ts == 0
  then 0
  else L.maximum $ S.toList $ S.map (\m -> monoDegree var m) ts

pseudoRemainder :: String -> Polynomial -> Polynomial -> Polynomial
pseudoRemainder s f g =
  let (_, _, r) = pseudoDivide s f g in
   r

pseudoDivide :: String -> Polynomial -> Polynomial -> (Polynomial, Polynomial, Polynomial)
pseudoDivide var f g =
  let b = pow (lcof var g) (L.maximum [(deg var f) - (deg var g) + 1, 0])
      (q, r) = rdivide var (times b f) g zero in
  (b, q, r)

divEven f g = do
  (q, r) <- divP f g
  case r == zero of
   True -> return q
   False -> Nothing

divP :: Polynomial -> Polynomial -> Maybe (Polynomial, Polynomial)
divP f g =
  case nextVar f g of
   Just v -> divide v f g
   Nothing -> do
     q <- intDiv f g
     return (q, zero)


divide :: String -> Polynomial -> Polynomial -> Maybe (Polynomial, Polynomial)
divide var f g =
  case g == zero of
   True -> Nothing
   False -> Just $ rdivide var f g zero

divideEvenly var f g = do
  (q, r) <- divide var f g
  case r == zero of
   True -> return q
   False -> Nothing

varSet (Polynomial ts) = S.unions $ L.map vars $ S.toList ts

nextVar f g =
  let vars = S.union (varSet f) (varSet g) in
  case S.size vars of
   0 -> Nothing
   _ -> Just $ S.findMax vars

getCon :: Polynomial -> Integer
getCon p@(Polynomial m) =
  if S.size m == 0
  then 0
  else monoCoeff $ S.findMax m

intDiv :: Polynomial -> Polynomial -> Maybe Polynomial
intDiv p1@(Polynomial m1) p2@(Polynomial m2) =
  if S.size m1 /= 1 || S.size m2 /= 1
  then error $ "intDiv: Something Bad " ++ show p1 ++ "\n" ++ show p2
  else
    let a = monoCoeff $ S.findMax m1
        b = monoCoeff $ S.findMax m2 in
     case rem a b of
      0 -> Just $ mkPoly [mkMono (div a b) []]
      _ -> Nothing

rdivide :: String -> Polynomial -> Polynomial -> Polynomial -> (Polynomial, Polynomial)
rdivide var f g q =
  if f == zero || deg var f < deg var g
  then (q, f)
  else
    case nextRes var f g of
     Just res ->
       let c = times (mkPoly [mkMono 1 [(var, deg var f - deg var g)]]) res
           qn = plus q c
           fn = minus f (times c g) in
        rdivide var fn g qn
     Nothing -> (q, f)

nextRes var f g =
    let lcf = lcof var f
        lcg = lcof var g in
     case nextVar lcf lcg of
      Just nextVar -> divideEvenly nextVar lcf lcg
      Nothing -> intDiv lcf lcg

derivative :: String -> Polynomial -> Polynomial
derivative var (Polynomial ms) =
  mkPolyS $ S.map (\m -> setMonoDegree var ((monoDegree var m) - 1) $ monomialTimes (monoDegree var m) m) $ S.filter (\m -> monoDegree var m /= 0) ms

isPos :: Polynomial -> Bool
isPos (Polynomial monos) = L.all (\m -> (monoCoeff m) > 0 && (evenPowers m)) monos

-- Very weak monicization for the simplifier
tryMonicize p@(Polynomial m) =
  case S.toList m of
   [Monomial c m] -> mkPoly [mkMonoMap 1 m]
   m -> p
  
