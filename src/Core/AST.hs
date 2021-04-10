module Core.AST
    ( Expr(..)
    , Literal(..)
    ) where

import Data.Text (Text)

data Expr a
  = Nil
  | Sym Text
  | Lit (Literal a)
  | List [Expr a]
  deriving (Eq, Show)

data Literal a
  = Fixnum Int
  | String a
  | Char Char
  | Bool Bool
  deriving (Eq, Show)
