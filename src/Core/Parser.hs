{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Core.Parser
  ( parse
  , fixnumMin
  , fixnumMax
  ) where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import Text.Megaparsec hiding (parse)

import Core.AST
import Core.Token (Parser)
import Core.Token qualified as T

nil :: Parser (Expr Text)
nil = T.openBrace >> T.closeBrace $> Nil

bool :: Parser (Literal Text)
bool = Bool <$> T.bool

fixnumMin, fixnumMax :: Int
fixnumMin = -536870912
fixnumMax =  536870911

number :: Parser (Literal Text)
number = do
  n <- T.number
  guard $ n <= fixnumMax
  guard $ n >= fixnumMin
  pure $ Fixnum n

char :: Parser (Literal Text)
char = Char <$> T.char

string :: Parser (Literal Text)
string = String <$> T.string

literal :: Parser (Expr Text)
literal = Lit <$> (bool <|> number <|> char <|> string)

sym :: Parser (Expr Text)
sym = Sym <$> T.symbol

list :: Parser (Expr Text)
list = List <$> (T.openBrace *> some expr <* T.closeBrace)

expr :: Parser (Expr Text)
expr = try nil <|> literal <|> sym <|> list

program :: Parser [Expr Text]
program = many expr <* eof

parse :: [T.Token] -> Either String [Expr Text]
parse = mapLeft show . runParser program "<input>"
