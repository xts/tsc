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

nil :: Parser Expr
nil = L.lexeme C.space (C.string "()") >> pure Nil

ident :: Parser String
ident = try special <|> try short <|> normal
  where
    special = do
      x <- C.char '*' <|> C.char '+'
      y <- some C.letterChar
      z <- many (C.alphaNumChar <|> C.symbolChar <|> C.punctuationChar )
      pure $ x : y ++ z
    short = (:[]) <$> (C.char '*' <|> C.char '+' <|> C.char '-' <|> C.char '/') <* C.space
    normal = do
      x <- C.letterChar
      y <- many (C.alphaNumChar <|> C.symbolChar <|> C.punctuationChar)
      pure $ x : y

sym :: Parser Expr
sym = Sym . pack <$> L.lexeme C.space ident

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
  sign <- optional (C.char '-' <|> C.char '+')
  n <- applySign sign <$> L.decimal
  guard $ n >= fixnumMin
  guard $ n <= fixnumMax
  pure $ Fixnum n

string :: Parser Literal
string = do
  _ <- C.char '"'
  s <- manyTill C.printChar (C.char '"')
  pure $ String $ pack s

literal :: Parser Expr
literal = Lit <$> L.lexeme C.space (bool <|> try fixnum <|> string)

list :: Parser Expr
list = List <$> (L.lexeme C.space (C.char '(') *> some expr <* optional C.space <* C.char ')')

expr :: Parser Expr
expr = nil <|> try literal <|> sym <|> list

program :: Parser Expr
program = expr <* eof

parse :: Text -> Either String Expr
parse = mapLeft errorBundlePretty . runParser program "<input>"
