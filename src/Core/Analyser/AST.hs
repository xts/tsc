module Core.Analyser.AST
  ( Expr(..)
  , Literal(..)
  , Label(..)
  , toList
  , sym
  , mapExpr
  ) where

import Data.Text (Text)

newtype Label = Label { unLabel :: Text }
  deriving (Eq, Show)

data Expr
  = Nil
  | Sym Text
  | Arg Int
  | Lit Literal
  | List [Expr]
  | Let [(Text, Expr)] [Expr]
  | Lam Label
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

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (List es) = List $ map (mapExpr f) es
mapExpr f e         = f e
