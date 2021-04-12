{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

import Core.Parser.AST
import Core.Lexer qualified as L
import Core.Parser qualified as P

parse :: Text -> Either String [Expr]
parse source = L.lex source >>= P.parse

spec :: Spec
spec = do
  describe "parse" $ do
    -- Nil.
    it "parses ()" $ do
      parse "()" `shouldBe` Right [Nil]

    -- Bool.
    it "parses bool" $ do
      parse "#t" `shouldBe` Right [Lit $ Bool True]
      parse "#f" `shouldBe` Right [Lit $ Bool False]

    -- Char.
    it "parses char" $ do
      parse "#\\a" `shouldBe` Right [Lit $ Char 'a']
      parse "#\\Z" `shouldBe` Right [Lit $ Char 'Z']

    -- Fixnum.
    it "parses fixnum" $ do
      parse "0" `shouldBe` Right [Lit $ Fixnum 0]
      parse "1" `shouldBe` Right [Lit $ Fixnum 1]
      parse "-1" `shouldBe` Right [Lit $ Fixnum (-1)]
      -- Test limits.
      parse "-536870912" `shouldBe` Right [Lit $ Fixnum (-536870912)]
      parse "536870911" `shouldBe` Right [Lit $ Fixnum 536870911]
      -- Going beyond limits should fail.
      parse "536870912" `shouldSatisfy` isLeft
      parse "-536870913" `shouldSatisfy` isLeft

    -- String.
    it "parses string" $ do
      parse "\"\"" `shouldBe` Right [Lit $ String ""]
      parse "\"Hello\"" `shouldBe` Right [Lit $ String "Hello"]
      parse "\"hello, world\"" `shouldBe` Right [Lit $ String "hello, world"]

    -- Symbol.
    it "parses symbol" $ do
      parse "+" `shouldBe` Right [Sym "+"]
      parse "*" `shouldBe` Right [Sym "*"]
      parse "-" `shouldBe` Right [Sym "-"]
      parse "+special+" `shouldBe` Right [Sym "+special+"]
      parse "*special*" `shouldBe` Right [Sym "*special*"]
      parse "here->there" `shouldBe` Right [Sym "here->there"]
      parse "hello" `shouldBe` Right [Sym "hello"]

    -- List.
    it "parses list" $ do
      parse "(1)" `shouldBe` Right [List [Lit (Fixnum 1)]]
      parse "( 1 )" `shouldBe` Right [List [Lit (Fixnum 1)]]
      parse "(1 2)" `shouldBe` Right [List [Lit (Fixnum 1), Lit (Fixnum 2)]]
      parse "(1 ())" `shouldBe` Right [List [Lit (Fixnum 1), Nil]]
      parse "(+ \"hello\" 1)" `shouldBe` Right [List [Sym "+", Lit (String "hello"), Lit (Fixnum 1)]]

    -- If.
    it "parses if" $ do
      parse "(if #t 1 0)" `shouldBe` Right [If (Lit $ Bool True) (Lit $ Fixnum 1) (Lit $ Fixnum 0)]
      parse "(if #t 1)" `shouldBe` Right [If (Lit $ Bool True) (Lit $ Fixnum 1) Nil]

    -- Let.
    it "parses let" $ do
      parse "(let ((x 2)) (* x x))" `shouldBe` Right
        [Let [("x", Lit (Fixnum 2))] [List [Sym "*", Sym "x", Sym "x"]]]
      parse "(let (x y) x y)" `shouldBe` Right [Let [("x", Nil), ("y", Nil)] [Sym "x", Sym "y"]]
      parse "(let () 1)" `shouldBe` Right [Let [] [Lit (Fixnum 1)]]
      parse "(let (x))" `shouldSatisfy` isLeft
      parse "(let)" `shouldSatisfy` isLeft
      parse "(let 1 x)" `shouldSatisfy` isLeft
      parse "(let \"x\" x)" `shouldSatisfy` isLeft

    -- Lambda.
    it "parser lambda" $ do
      parse "(lambda () ())" `shouldBe` Right [Lam [] [Nil]]
      parse "(lambda (x) x)" `shouldBe` Right [Lam ["x"] [Sym "x"]]
      parse "(lambda (x y) x y)" `shouldBe` Right [Lam ["x", "y"] [Sym "x", Sym "y"]]
      parse "(lambda ())" `shouldSatisfy` isLeft
      parse "(lambda 1 ())" `shouldSatisfy` isLeft
      parse "(lambda \"x\" ())" `shouldSatisfy` isLeft
      parse "(lambda ((x 1)) ())" `shouldSatisfy` isLeft


    -- Multiple expressions.
    it "parses multiple expressions" $ do
      parse "()()" `shouldBe` Right [Nil, Nil]
