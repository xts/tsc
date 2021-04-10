module Core.Analyser.AST
  ( Expr(..)
  , Literal(..)
  , Label(..)
  ) where

import Data.Text (Text)

newtype Label = Label Text
  deriving (Eq, Show)

data Expr
  = Nil
  | Sym Text
  | Lit Literal
  | List [Expr]
  deriving (Eq, Show)

data Literal
  = Fixnum Int
  | String Label
  | Char Char
  | Bool Bool
  deriving (Eq, Show)
