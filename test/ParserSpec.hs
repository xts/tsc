{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Core.AST
import Core.Parser

spec :: Spec
spec = do
  describe "parse" $ do
    -- Nil.
    it "parses ()" $ do
      parse "()" `shouldBe` Right (Lit Nil)

    -- Bool.
    it "parses bool" $ do
      parse "#t" `shouldBe` Right (Lit $ Bool True)
      parse "#f" `shouldBe` Right (Lit $ Bool False)

    -- Fixnum.
    it "parses fixnum" $ do
      parse "0" `shouldBe` Right (Lit $ Fixnum 0)
      parse "1" `shouldBe` Right (Lit $ Fixnum 1)
      parse "1073741823" `shouldBe` Right (Lit $ Fixnum 1073741823)

    -- String.
    it "parser string" $ do
      parse "\"\"" `shouldBe` Right (Lit $ String "")
      parse "\"Hello\"" `shouldBe` Right (Lit $ String "Hello")
      parse "\"hello, world\"" `shouldBe` Right (Lit $ String "hello, world")
