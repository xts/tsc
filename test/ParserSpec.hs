{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)

import Core.AST
import Core.Parser

spec :: Spec
spec = do
  describe "parse" $ do
    -- Nil.
    it "parses ()" $ do
      parse "()" `shouldBe` Right Nil

    -- Bool.
    it "parses bool" $ do
      parse "#t" `shouldBe` Right (Lit $ Bool True)
      parse "#f" `shouldBe` Right (Lit $ Bool False)

    -- Fixnum.
    it "parses fixnum" $ do
      parse "0" `shouldBe` Right (Lit $ Fixnum 0)
      parse "1" `shouldBe` Right (Lit $ Fixnum 1)
      parse "-1" `shouldBe` Right (Lit $ Fixnum (-1))
      -- Test limits.
      parse "-536870912" `shouldBe` Right (Lit $ Fixnum (-536870912))
      parse "536870911" `shouldBe` Right (Lit $ Fixnum 536870911)
      -- Going beyond limits should fail.
      parse "536870912" `shouldSatisfy` isLeft
      parse "-536870913" `shouldSatisfy` isLeft

    -- String.
    it "parses string" $ do
      parse "\"\"" `shouldBe` Right (Lit $ String "")
      parse "\"Hello\"" `shouldBe` Right (Lit $ String "Hello")
      parse "\"hello, world\"" `shouldBe` Right (Lit $ String "hello, world")

    -- Symbol.
    it "parses symbol" $ do
      parse "+" `shouldBe` Right (Sym "+")
      parse "*" `shouldBe` Right (Sym "*")
      parse "-" `shouldBe` Right (Sym "-")
      parse "+special+" `shouldBe` Right (Sym "+special+")
      parse "*special*" `shouldBe` Right (Sym "*special*")
      parse "here->there" `shouldBe` Right (Sym "here->there")
      parse "hello" `shouldBe` Right (Sym "hello")

    -- List.
    it "parses list" $ do
      parse "(1)" `shouldBe` Right (List [Lit (Fixnum 1)])
      parse "( 1 )" `shouldBe` Right (List [Lit (Fixnum 1)])
      parse "(1 2)" `shouldBe` Right (List [Lit (Fixnum 1), Lit (Fixnum 2)])
      parse "(1 ())" `shouldBe` Right (List [Lit (Fixnum 1), Nil])
      parse "(+ \"hello\" 1)" `shouldBe` Right (List [Sym "+", Lit (String "hello"), Lit (Fixnum 1)])
