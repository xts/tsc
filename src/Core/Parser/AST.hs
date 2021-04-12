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
  | Let [(Text, Expr)] [Expr]
  deriving (Eq, Show)

data Literal
  = Bool Bool
  | Char Char
  | Fixnum Int
  | String Text
  deriving (Eq, Show)
