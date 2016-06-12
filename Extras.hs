module Extras(quadratic, cubic) where

import Polynomial

quadratic = mkPoly [mkMono 1 [("a", 1), ("x", 2)], mkMono 1 [("b", 1), ("x", 1)], mkMono 1 [("c", 1)]]

cubic = mkPoly [mkMono 1 [("a", 1), ("x", 3)],
                mkMono 1 [("b", 1), ("x", 2)],
                mkMono 1 [("c", 1), ("x", 1)],
                mkMono 1 [("d", 1)]]
