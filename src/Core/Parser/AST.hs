module Core.Parser.AST
    ( Expr(..)
    , Literal(..)
    ) where

import Data.Text (Text)

data Expr
  = Nil
  | Sym Text
  | Lit Literal
  | List [Expr]
  deriving (Eq, Show)

data Literal
  = Fixnum Int
  | String Text
  | Char Char
  | Bool Bool
  deriving (Eq, Show)
