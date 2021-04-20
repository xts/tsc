module RenamerSpec (spec) where

import Test.Hspec

import Core.AST
import Core.Parser
import Core.Transformers.Renamer qualified as R

rename :: Text -> Either String [Expr]
rename s = R.rename =<< parse s

spec :: Spec
spec = do
  describe "rename" $ do

    it "renames let-bound symbols" $ do
      rename "(let (x) x)" `shouldBe` Right [Let [Binding (Name "x0") Nil] [Sym "x0"]]
      rename "(let (x y) x y)" `shouldBe` Right [Let [Binding (Name "x0") Nil, Binding (Name "y0") Nil] [Sym "x0", Sym "y0"]]

    it "renames lambda parameters" $ do
      rename "(lambda (x y) x)" `shouldBe` Right [LamDef (Args ["x0", "y0"]) (FreeArgs []) [Sym "x0"]]

    it "prevents shadowing" $ do
      rename "(let (x) (+ x (let (x) x)) x)" `shouldBe`
        Right [Let [Binding (Name "x0") Nil] [List [Sym "+", Sym "x0", Let [Binding (Name "x1") Nil] [Sym "x1"]], Sym "x0"]]

    it "catches unbound symbols" $ do
      rename "(let (x) y)" `shouldSatisfy` isLeft

    it "ignores primitives" $ do
      rename "(let (x) (+ x x))" `shouldBe` Right [Let [Binding (Name "x0") Nil] [List [Sym "+", Sym "x0", Sym "x0"]]]
