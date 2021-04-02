{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Either.Combinators (mapLeft)
import Data.Text (Text, pack)
import Data.Void (Void)

type Parser = Parsec Void Text

nil :: Parser Literal
nil = do
  _ <- C.string "()"
  pure Nil

bool :: Parser Literal
bool = Bool <$> (true <|> false)
  where
    true  = C.string "#t" >> pure True
    false = C.string "#f" >> pure False

fixnum :: Parser Literal
fixnum = Fixnum <$> L.decimal

string :: Parser Literal
string = do
  _ <- C.char '"'
  s <- manyTill (C.printChar) (C.char '"')
  pure $ String $ pack s

literal :: Parser Literal
literal = bool <|> fixnum <|> string <|> nil

program :: Parser AST
program = Lit <$> literal <* eof

parse :: Text -> Either String AST
parse = mapLeft errorBundlePretty . runParser program "<input>"
