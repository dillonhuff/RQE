module Polynomial(mkPoly, mkMono, one, zero,
                  lcof,
                  plus, times,
                  pseudoDivide) where

import Data.List as L
import Data.Map as M
import Data.Set as S

data Monomial = Monomial Integer (Map String Integer)
                deriving (Eq, Ord)

mkMono :: Integer -> [(String, Integer)] -> Monomial
mkMono i vars = Monomial i (M.fromList vars)

monomialTimes :: Integer -> Monomial -> Monomial
monomialTimes i (Monomial c ms) = Monomial (i*c) ms

deleteVar :: String -> Monomial -> Monomial
deleteVar x (Monomial c vars) = Monomial c $ M.delete x vars

monoDegree var (Monomial _ vars) =
  case M.lookup var vars of
   Just d -> d
   Nothing -> 0

--  case M.lookup
instance Show Monomial where
  show (Monomial c vars) =
    let varList = M.toList vars in
     if c /= 1 && (length varList) /= 0
     then show c ++ "*" ++ (printVars $ M.toList vars)
     else if length varList == 0
          then "1"
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
mkPoly monomials = Polynomial (S.fromList monomials)

timesInt :: Integer -> Polynomial -> Polynomial
timesInt i (Polynomial ms) = Polynomial (S.map (\m -> monomialTimes i m) ms)

times :: Polynomial -> Polynomial -> Polynomial
times p q = p

plus :: Polynomial -> Polynomial -> Polynomial
plus (Polynomial m1) (Polynomial m2) = Polynomial $ S.fromList $ simplify ((S.toList m1) ++ (S.toList m2))

simplify :: [Monomial] -> [Monomial]
simplify ms =
  let sorted = sortBy (\(Monomial _ m1) (Monomial _ m2) -> compare m1 m2) ms
      grouped = groupBy (\(Monomial _ m1) (Monomial _ m2) -> m1 == m2) sorted
      mPlus = (\(Monomial c1 m1) (Monomial c2 _) -> Monomial (c1 + c2) m1)
      results = L.map (\terms -> L.foldr mPlus (L.head terms) (L.tail terms)) grouped in
   results

pow :: Polynomial -> Integer -> Polynomial
pow p 0 = mkPoly [mkMono 1 []]
pow p 1 = p
pow p n = pow (times p p) (n-1)

lcof :: String -> Polynomial -> Polynomial
lcof var p@(Polynomial ts) = Polynomial $ S.map (deleteVar var) $ S.filter (\m -> (monoDegree var m) == (deg var p)) ts

deg :: String -> Polynomial -> Integer
deg var (Polynomial ts) =
  L.maximum $ S.toList $ S.map (\m -> monoDegree var m) ts

pseudoDivide :: String -> Polynomial -> Polynomial -> (Polynomial, Polynomial, Polynomial)
pseudoDivide var f g = (pow (lcof var g) ((deg var f) - (deg var g) + 1), f, f)
