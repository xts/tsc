{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}

module Core.Parser
  ( parse
  , fixnumMin
  , fixnumMax
  ) where

import Data.Either.Combinators (mapLeft)
import Data.Text (pack)
import Text.Megaparsec hiding (parse, token, tokens, many)
import Text.Megaparsec.Char hiding (char, string)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (bool, some)

import Core.Parser.AST

type Parser = Parsec Void Text

nil :: Parser Expr
nil = openBrace >> closeBrace $> Nil

fixnumMin, fixnumMax :: Int
fixnumMin = -536870912
fixnumMax =  536870911

number :: Parser Literal
number = do
  n <- L.signed (pure ()) (lexeme L.decimal)
  guard $ n <= fixnumMax
  guard $ n >= fixnumMin
  pure $ Fixnum n

literal :: Parser Expr
literal = Lit <$> (bool <|> number <|> char <|> string)

char :: Parser Literal
char = Char <$> (L.symbol sc "#\\" *> printChar)

string :: Parser Literal
string = String . pack <$> lexeme (C.char '\"' *> manyTill L.charLiteral (C.char '\"'))

ifForm :: Parser Expr
ifForm = do
  _ <- lexeme $ C.string "if"
  cond <- expr
  t <- expr
  f <- expr <|> lookAhead closeBrace $> Nil
  pure $ If cond t f

letForm :: Parser Expr
letForm = do
  _ <- lexeme $ C.string "let"
  _ <- openBrace
  vars <- many letVar
  _ <- closeBrace
  body <- some expr
  pure $ Let vars body

letVar :: Parser (Text, Expr)
letVar = letNil <|> letExpr
  where
    letNil = do
      s <- sym
      pure (s, Nil)
    letExpr = do
      _ <- openBrace
      s <- sym
      e <- expr
      _ <- closeBrace
      pure (s, e)

lambda :: Parser Expr
lambda = do
  _ <- lexeme $ C.string "lambda"
  _ <- openBrace
  params <- many sym
  _ <- closeBrace
  body <- some expr
  pure $ Lam params body

form :: Parser Expr
form = openBrace *> (ifForm <|> letForm <|> lambda <|> List <$> some expr) <* closeBrace

expr :: Parser Expr
expr = try nil <|> Sym <$> sym <|> literal <|> form

program :: Parser [Expr]
program = many expr <* eof

parse :: Text -> Either String [Expr]
parse = mapLeft errorBundlePretty . runParser program "<input>"

-- | Swallow any whitespace or comment.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Swallow space and apply given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

bool :: Parser Literal
bool = Bool <$> (true <|> false)
  where
    true  = L.symbol sc "#t" $> True
    false = L.symbol sc "#f" $> False

openBrace :: Parser ()
openBrace = L.symbol sc "(" $> ()

closeBrace :: Parser ()
closeBrace = L.symbol sc ")" $> ()

sym :: Parser Text
sym = pack <$> lexeme (try special <|> normal)
  where
    -- Symbol that could be mistaken for a number.
    special = do
      a <- symChar
      notFollowedBy digitChar
      b <- many (alphaNumChar <|> symChar)
      pure $ a : b

    normal = do
      a <- letterChar
      b <- many (alphaNumChar <|> symChar)
      pure $ a : b

    symChar = C.char '*' <|> C.char '/' <|> C.char '+' <|> C.char '-' <|> C.char '<' <|> C.char '>' <|> C.char '='
