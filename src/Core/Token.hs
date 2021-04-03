module Core.Token
  ( Token(..)
  , Parser
  , bool
  , number
  , symbol
  , char
  , string
  , openBrace
  , closeBrace
  , quote
  ) where

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec (Parsec, satisfy, single, (<?>))

-- | Our lexer output.
data Token
  = Bool Bool
  | Number Int
  | Symbol Text
  | Char Char
  | String Text
  | OpenBrace
  | CloseBrace
  | Quote
  deriving (Eq, Ord, Show)

type Parser = Parsec Void [Token]

bool :: Parser Bool
bool = (satisfy isBool <?> "bool") >>= \(Bool b) -> pure b
  where
    isBool (Bool _) = True
    isBool _        = False

number :: Parser Int
number = (satisfy isNumber <?> "number") >>= \(Number n) -> pure n
  where
    isNumber (Number _) = True
    isNumber _          = False

symbol :: Parser Text
symbol = (satisfy isSymbol <?> "symbol") >>= \(Symbol s) -> pure s
  where
    isSymbol (Symbol _) = True
    isSymbol _          = False

char :: Parser Char
char = (satisfy isChar <?> "char") >>= \(Char c) -> pure c
  where
    isChar (Char _) = True
    isChar _        = False

string :: Parser Text
string = (satisfy isString <?> "string") >>= \(String s) -> pure s
  where
    isString (String _) = True
    isString _          = False

openBrace :: Parser Token
openBrace = single OpenBrace

closeBrace :: Parser Token
closeBrace = single CloseBrace

quote :: Parser Token
quote = single Quote
