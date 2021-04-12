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

import Core.Parser.AST
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

ifForm :: Parser Expr
ifForm = do
  _ <- single (T.Symbol "if")
  cond <- expr
  t <- expr
  f <- expr <|> lookAhead T.closeBrace $> Nil
  pure $ If cond t f

letForm :: Parser Expr
letForm = do
  _ <- single (T.Symbol "let")
  _ <- T.openBrace
  vars <- many letVar
  _ <- T.closeBrace
  body <- some expr
  pure $ Let vars body

letVar :: Parser (Text, Expr)
letVar = letNil <|> letExpr
  where
    letNil = do
      (Sym s) <- sym
      pure (s, Nil)
    letExpr = do
      _ <- T.openBrace
      (Sym s) <- sym
      e <- expr
      _ <- T.closeBrace
      pure (s, e)

lambda :: Parser Expr
lambda = do
  _ <- single (T.Symbol "lambda")
  _ <- T.openBrace
  params <- many T.symbol
  _ <- T.closeBrace
  body <- some expr
  pure $ Lam params body

form :: Parser Expr
form = T.openBrace *> (ifForm <|> letForm <|> lambda <|> List <$> some expr) <* T.closeBrace

expr :: Parser Expr
expr = try nil <|> literal <|> sym <|> form

program :: Parser [Expr]
program = many expr <* eof

parse :: [T.Token] -> Either String [Expr]
parse = mapLeft show . runParser program "<input>"
