module Polynomial(mkPoly, mkMono, one, zero,
                  lcof,
                  plus, times, divide,
                  pseudoDivide) where

import Data.List as L
import Data.Map as M
import Data.Set as S

data Monomial = Monomial Integer (Map String Integer)
                deriving (Eq, Ord)

mkMono :: Integer -> [(String, Integer)] -> Monomial
mkMono i vars = Monomial i (M.fromList $ L.filter (\(_, c) -> c /= 0) vars)

monoCoeff (Monomial c _) = c

vars :: Monomial -> Set String
vars (Monomial _ vs) = M.keysSet vs

monomialTimes :: Integer -> Monomial -> Monomial
monomialTimes i (Monomial c ms) = Monomial (i*c) ms

monomialProd :: Monomial -> Monomial -> Monomial
monomialProd (Monomial c1 m1) (Monomial c2 m2) =
  Monomial (c1*c2) $ M.unionWith (\i j -> i + j) m1 m2

deleteVar :: String -> Monomial -> Monomial
deleteVar x (Monomial c vars) = Monomial c $ M.delete x vars

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
printVars ((x, p):[]) =
  x ++ "^" ++ (show p)
printVars ((x, p):(y, q):rest) =
  x ++ "^" ++ (show p) ++ "*" ++
  y ++ "^" ++ (show q) ++ (printVars rest)

data Polynomial = Polynomial (Set Monomial)
                  deriving (Eq, Ord)

instance Show Polynomial where
  show (Polynomial rs) =
    let mList = S.toList rs in
     if length mList == 0
     then "0"
     else sumList $ S.toList rs

one = mkPoly [mkMono 1 []]
zero = mkPoly [mkMono 0 []]

sumList :: Show a => [a] -> String
sumList [] = ""
sumList [x] = show x
sumList (x:y:rest) = show x ++ " + " ++  show y ++ (sumList rest)

mkPoly :: [Monomial] -> Polynomial
mkPoly monomials = Polynomial $ S.fromList $ L.filter (\m -> monoCoeff m /= 0) monomials

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

pseudoDivide :: String -> Polynomial -> Polynomial -> (Polynomial, Polynomial, Polynomial)
pseudoDivide var f g =
  let b = pow (lcof var g) (L.maximum [(deg var f) - (deg var g) + 1, 0])
      (q, r) = rdivide var (times b f) g zero in
  (b, q, r)

divide :: String -> Polynomial -> Polynomial -> Maybe (Polynomial, Polynomial)
divide var f g = Just $ rdivide var f g zero

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
        --(qn, fn)
        rdivide var fn g qn --(qn, fn)
--        (plus q c, minus f (times c g)) --rdivide var (minus f (times res g)) g (plus q res)
     Nothing -> (q, f)

nextRes var f g =
    let lcf = lcof var f
        lcg = lcof var g in
     case nextVar lcf lcg of
      Just nextVar -> divideEvenly nextVar lcf lcg
      Nothing -> intDiv lcf lcg
