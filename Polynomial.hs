module Polynomial(mkPoly, mkMono,
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

instance Show Monomial where
  show (Monomial c vars) = show c ++ "*" ++ (printVars $ M.toList vars)

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
  show (Polynomial rs) = show rs

mkPoly :: [Monomial] -> Polynomial
mkPoly monomials = Polynomial (S.fromList monomials)

times :: Integer -> Polynomial -> Polynomial
times i (Polynomial ms) = Polynomial (S.map (\m -> monomialTimes i m) ms)

plus :: Polynomial -> Polynomial -> Polynomial
plus (Polynomial m1) (Polynomial m2) = Polynomial $ S.fromList $ simplify ((S.toList m1) ++ (S.toList m2))

simplify :: [Monomial] -> [Monomial]
simplify ms =
  let sorted = sortBy (\(Monomial _ m1) (Monomial _ m2) -> compare m1 m2) ms
      grouped = groupBy (\(Monomial _ m1) (Monomial _ m2) -> m1 == m2) sorted
      mPlus = (\(Monomial c1 m1) (Monomial c2 _) -> Monomial (c1 + c2) m1)
      results = L.map (\terms -> L.foldr mPlus (L.head terms) (L.tail terms)) grouped in
   results

pseudoDivide :: String -> Polynomial -> Polynomial -> (Monomial, Polynomial, Polynomial)
pseudoDivide var f g = (mkMono 1 [], f, f)
