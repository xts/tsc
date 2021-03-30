module Lexer (lex) where

import Data.Text

data Lexeme = LNumber Integer
            | LChar Char
            | LBool Bool
            | LNil

lex :: Text -> [Lexeme]
