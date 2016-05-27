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

    it "with remainder" $ do
      let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
          g = mkPoly [mkMono 1 [("x", 1)], mkMono (-1) [("y", 1)]]
          b = mkMono 1 []
          q = mkPoly []
          r = mkPoly [] in
       pseudoDivide "x" f g `shouldBe` (one, q, r)
