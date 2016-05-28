module Logic(Formula(..),
             projectFormula,
             true, false,
             gtz) where

import Data.List as L

import Polynomial
import SignTable

data Formula a = Pred a
               | Binop String (Formula a) (Formula a)
               | Unop String (Formula a)
               | T
               | F
                 deriving (Eq, Ord, Show)

true = T
false = F

disjunction [] = error "disjunction"

atomUnion f = error "atomUnion"

data ArithPred = Pr String Polynomial
                 deriving (Eq, Ord, Show)

gtz p = Pred $ Pr ">" p

getPoly (Pr _ p) = p

projectFormula :: String -> Formula ArithPred -> Formula ArithPred
projectFormula var f =
  disjunction $ L.map fst $ L.filter (\(_, t) -> hasSatAssignment f t) $ signTables var $ L.map getPoly $ atomUnion f

hasSatAssignment :: Formula ArithPred -> SignTable -> Bool
hasSatAssignment f sts = error "satAssignments"

signTables :: String -> [Polynomial] -> [(Formula ArithPred, SignTable)]
signTables s ps = error "signTables"
