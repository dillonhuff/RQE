module Tests(main) where

import Test.Hspec

import Extras
import Logic
import Polynomial
import SignTable

main :: IO ()
main = hspec $ do
  describe "Polynomial addition" $ do

    it "add multivariate polynomials" $ do
      (plus
       (mkPoly [mkMono 2 [("x", 3), ("y", 4)]])
       (mkPoly [mkMono 5 [("x", 3)]])) `shouldBe`
        (mkPoly [mkMono 2 [("x", 3), ("y", 4)], mkMono 5 [("x", 3)]])

    it "add multivariate polynomials with simplification" $ do
      (plus
       (mkPoly [mkMono 2 [("x", 3), ("y", 4)]])
       (mkPoly [mkMono 5 [("x", 3), ("y", 4)]])) `shouldBe`
        (mkPoly [mkMono 7 [("x", 3), ("y", 4)]])

  describe "Leading coefficient" $ do
    it "Univariate, one term" $ do
      (lcof "x" $ mkPoly [mkMono 3 [("x", 4)]]) `shouldBe` mkPoly [mkMono 3 []]

    it "Multivariate, a few terms" $ do
    (lcof "x" $ mkPoly [mkMono 3 [("x", 4), ("z", 2)],
                        mkMono (-4) [("x", 2), ("y", 3)]])
      `shouldBe`
      (mkPoly [mkMono 3 [("z", 2)]])

  describe "Division" $ do
    it "Two univariate polynomials" $ do
      divide "x" (mkPoly [mkMono 4 [("x", 2)]]) (mkPoly [mkMono 1 [("x", 1)]])
      `shouldBe`
      Just (mkPoly [mkMono 4 [("x", 1)]], zero)

  describe "Pseudo division" $ do

    it "deg(f) > deg(g)" $ do
      let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
          g = mkPoly [mkMono 1 [("x", 7)], mkMono (-1) [("y", 1)]] in
       pseudoDivide "x" f g `shouldBe` (one, zero, f)

    it "Multivariate, b != 1, no remainder" $ do
      let f = mkPoly [mkMono 9 [("z", 5), ("x", 2), ("y", 2), ("w", 4)]]
          g = mkPoly [mkMono 2 [("z", 1), ("x", 2), ("y", 1)]]
          (b, q, r) = pseudoDivide "z" f g in
       (times b f) `shouldBe` (plus (times q g) r)

    it "with remainder" $ do
      let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
          g = mkPoly [mkMono 1 [("x", 1)], mkMono (-1) [("y", 1)]]
          q = mkPoly [mkMono 1 [("x", 1)], mkMono 1 [("y", 1)]]
          r = mkPoly [mkMono 2 [("y", 2)]] in
       pseudoDivide "x" f g `shouldBe` (one, q, r)

  describe "Derivative" $ do

    it "Multivariate polynomial" $ do
      let f = mkPoly [mkMono 9 [("z", 5), ("x", 2), ("y", 2), ("w", 4)],
                      mkMono 2 [("z", 1), ("x", 2), ("y", 1)]]
          fp = mkPoly [mkMono 36 [("z", 5), ("x", 2), ("y", 2), ("w", 3)]] in
       derivative "w" f `shouldBe` fp

  describe "Sign table computation" $ do

    it "Sign table for linear function has 1 entry" $ do
      let p = mkPoly [mkMono 7 [("z", 1)]]
          sts = signTables "z" [p] in
       length sts == 1

    it "Sign table contains polynomial it is computed for" $ do
      let p = mkPoly [mkMono 7 [("z", 1)]]
      elem p (columnLabels $ snd $ head $ signTables "z" [p]) `shouldBe` True

    it "Sign table for linear function gives signs" $ do
      let p = mkPoly [mkMono 7 [("z", 1)]]
          st = snd $ head $ signTables "z" [p] in
       selectSigns p st `shouldBe` [Neg, Zero, Pos]

  describe "Quantifier elimination" $ do

    it "All constants, false" $ do
      (projectFormula "x" (eqz (mkCon 3))) `shouldBe` false

    it "All constants, true" $ do
      (projectFormula "x" (gtz (mkCon 3))) `shouldBe` true

    it "All constants, true" $ do
      (projectFormula "x" (dis (eqz (mkCon (-3))) (gtz (mkCon 3)))) `shouldBe` true

    it "One Variable and one quantifier" $ do
      let f = mkPoly [mkMono 3 [("x", 1)]] in
       (projectFormula "x" (gtz f)) `shouldBe` true

    it "One variable, false" $ do
      let f = mkPoly [mkMono 3 [("x", 1)]] in
       (projectFormula "x" (con (gtz f) (ltz f))) `shouldBe` false

    it "One variable, quadratic, true" $ do
      let f = mkPoly [mkMono 5 [("x", 2)]] in
       (projectFormula "x" (dis (con (gtz f) (ltz f)) (eqz f))) `shouldBe` true

    it "Two variables, 6*x^2*y^2 < 0 is false" $ do
      let f = mkPoly [mkMono 6 [("x", 2), ("y", 2)]] in
       (projectFormula "y" $ projectFormula "x" (ltz f)) `shouldBe` false

    it "Not all quadratic equations have a root" $ do
       (projectFormula "x" (eqz quadratic)) `shouldNotBe` true
