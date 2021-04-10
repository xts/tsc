module Core.Analyser.AST
  ( Expr(..)
  , Literal(..)
  , Label(..)
  , sym
  , toList
  ) where

import Data.Text (Text)

newtype Label = Label { unLabel :: Text }
  deriving (Eq, Show)

data Expr
  = Nil
  | Sym Text
  | Lam Label
  | Lit Literal
  | List [Expr]
  deriving (Eq, Show)

data Literal
  = Fixnum Int
  | String Label
  | Char Char
  | Bool Bool
  deriving (Eq, Show)

toList :: Expr -> [Expr]
toList Nil       = []
toList (List es) = es
toList e         = error $ "Not list-like: " <> show e

sym :: Expr -> Text
sym (Sym s) = s
sym e       = error $ "Not a symbol: " <> show e
