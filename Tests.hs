module Tests(main) where

import Test.Hspec

import Polynomial

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

  describe "Pseudo division" $ do

    it "simple pseudo division with remainder" $ do
      let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
          g = mkPoly [mkMono 1 [("x", 1)], mkMono (-1) [("y", 1)]]
          b = mkMono 1 []
          q = mkPoly []
          r = mkPoly [] in
       pseudoDivide "x" f g `shouldBe` (b, q, r)
