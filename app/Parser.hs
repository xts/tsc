module Parser where

import AST
import Text.Megaparsec

type Parser = Parsec Void Text

parse :: Text -> AST
