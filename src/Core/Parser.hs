{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Core.Parser
  ( parse
  , fixnumMin
  , fixnumMax
  ) where

import Data.Functor (($>))
import Control.Monad (guard)
import Text.Megaparsec hiding (parse)
import Data.Either.Combinators (mapLeft)

import Core.AST
import Core.Token (Parser)
import Core.Token qualified as T

nil :: Parser Expr
nil = T.openBrace >> T.closeBrace $> Nil

bool :: Parser Literal
bool = Bool <$> T.bool

fixnumMin, fixnumMax :: Int
fixnumMin = -536870912
fixnumMax =  536870911

number :: Parser Literal
number = do
  n <- T.number
  guard $ n <= fixnumMax
  guard $ n >= fixnumMin
  pure $ Fixnum n

char :: Parser Literal
char = Char <$> T.char

string :: Parser Literal
string = String <$> T.string

literal :: Parser Expr
literal = Lit <$> (bool <|> number <|> char <|> string)

sym :: Parser Expr
sym = Sym <$> T.symbol

list :: Parser Expr
list = List <$> (T.openBrace *> some expr <* T.closeBrace)

expr :: Parser Expr
expr = try nil <|> literal <|> sym <|> list

program :: Parser Expr
program = expr <* eof

parse :: [T.Token] -> Either String Expr
parse = mapLeft show . runParser program "<input>"
