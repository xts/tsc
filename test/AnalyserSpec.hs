{-# LANGUAGE OverloadedStrings #-}

module AnalyserSpec (spec) where

import Test.Hspec

import Core.Analyser qualified as A
import Core.Analyser.AST
import Core.Parser (parse)
import Core.Renamer

analyse :: Text -> Either String ([Expr], A.Info)
analyse s = A.analyse <$> (rename =<< parse s)

hasStrings :: Int -> Either String ([Expr], A.Info) -> Bool
hasStrings n (Right (_, A.Info strings _)) = length strings == n
hasStrings _ _                             = False

hasLambdas :: Int -> Either String ([Expr], A.Info) -> Bool
hasLambdas n (Right (_, A.Info _ lambdas)) = length lambdas == n
hasLambdas _ _                             = False

type Lambda = ([Text], [Text])

lambda :: [Lambda] -> Either String ([Expr], A.Info) -> Bool
lambda ls (Right (_, A.Info _ lambdas)) = all go $ zip ls $ map fst lambdas
  where go (l, lam) = l == (A.lmParams lam, A.lmFree lam)
lambda _  _                             = False

spec :: Spec
spec = do
  describe "analyse" $ do

    it "captures strings" $ do
      analyse "" `shouldSatisfy` hasStrings 0
      analyse "\"hello\"" `shouldSatisfy` hasStrings 1
      analyse "\"hello\" \"world\"" `shouldSatisfy` hasStrings 2

    it "captures lambdas" $ do
      analyse "" `shouldSatisfy` hasLambdas 0
      analyse "(lambda (x) x)" `shouldSatisfy` hasLambdas 1
      analyse "(lambda (x) x) (lambda (x) x)" `shouldSatisfy` hasLambdas 2
      analyse "(lambda (x) (lambda (x) x))" `shouldSatisfy` hasLambdas 2

    it "captures lambda parameters" $ do
      analyse "(lambda () ())" `shouldSatisfy` lambda [([], [])]
      analyse "(lambda (x) x)" `shouldSatisfy` lambda [(["x0"], [])]
      analyse "(lambda (x y) x)" `shouldSatisfy` lambda [(["x0", "y0"], [])]

    it "captures free variables" $ do
      analyse "(let (x) (lambda (y) x))" `shouldSatisfy` lambda [(["y0"], ["x0"])]
      analyse "(let (x) (lambda (y) y))" `shouldSatisfy` lambda [(["y0"], [])]
      analyse "(lambda (y) (lambda () y))" `shouldSatisfy` lambda [(["y0"], []), ([], ["y0"])]

    it "propagates free variables from child lambdas" $ do
      analyse "(let (x) (lambda (y) (lambda () x y)))" `shouldSatisfy` lambda [(["y0"], ["x0"]), ([], ["x0", "y0"])]
