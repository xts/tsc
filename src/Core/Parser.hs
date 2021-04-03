{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Core.Parser
  ( parse
  , fixnumMin
  , fixnumMax
  ) where

import Control.Monad (guard)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Either.Combinators (mapLeft)
import Data.Text (Text, pack)
import Data.Void (Void)

import Core.AST

type Parser = Parsec Void Text

fixnumMin, fixnumMax :: Int
fixnumMin = -536870912
fixnumMax =  536870911

nil :: Parser Literal
nil = do
  _ <- C.string "()"
  pure Nil

bool :: Parser Literal
bool = Bool <$> (true <|> false)
  where
    true  = C.string "#t" >> pure True
    false = C.string "#f" >> pure False

applySign :: Maybe Char -> Int -> Int
applySign (Just '-') n = -n
applySign _          n =  n

fixnum :: Parser Literal
fixnum = do
  neg <- optional $ C.char '-'
  n <- applySign neg <$> L.decimal
  guard $ n >= fixnumMin
  guard $ n <= fixnumMax
  pure $ Fixnum n

string :: Parser Literal
string = do
  _ <- C.char '"'
  s <- manyTill C.printChar (C.char '"')
  pure $ String $ pack s

literal :: Parser Literal
literal = L.lexeme C.space (bool <|> fixnum <|> string <|> nil)

program :: Parser AST
program = Lit <$> literal <* eof

parse :: Text -> Either String AST
parse = mapLeft errorBundlePretty . runParser program "<input>"
