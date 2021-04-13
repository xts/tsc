{-# LANGUAGE OverloadedStrings #-}

module RenamerSpec (spec) where

import Test.Hspec

import Core.Parser.AST
import Core.Parser
import Core.Renamer qualified as R

rename :: Text -> Either String [Expr]
rename s = R.rename =<< parse s

spec :: Spec
spec = do
  describe "rename" $ do

    it "renames let-bound symbols" $ do
      rename "(let (x) x)" `shouldBe` Right [Let [("x0", Nil)] [Sym "x0"]]
      rename "(let (x y) x y)" `shouldBe` Right [Let [("x0", Nil), ("y0", Nil)] [Sym "x0", Sym "y0"]]

    it "renames lambda parameters" $ do
      rename "(lambda (x y) x)" `shouldBe` Right [Lam ["x0", "y0"] [Sym "x0"]]

    it "prevents shadowing" $ do
      rename "(let (x) (+ x (let (x) x)) x)" `shouldBe`
        Right [Let [("x0", Nil)] [List [Sym "+", Sym "x0", Let [("x1", Nil)] [Sym "x1"]], Sym "x0"]]

    it "catches unbound symbols" $ do
      rename "(let (x) y)" `shouldSatisfy` isLeft

    it "ignores primitives" $ do
      rename "(let (x) (+ x x))" `shouldBe` Right [Let [("x0", Nil)] [List [Sym "+", Sym "x0", Sym "x0"]]]
