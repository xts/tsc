module Core.Parser.AST
    ( Expr(..)
    , Literal(..)
    , letVars
    ) where

import Data.Text (Text)

data Expr
  = Nil
  | Sym Text
  | Lit Literal
  | List [Expr]
  deriving (Eq, Show)

data Literal
  = Bool Bool
  | Char Char
  | Fixnum Int
  | String Text
  deriving (Eq, Show)

letVars :: [Expr] -> [(Text, Expr)]
letVars (List [Sym s, e] : vs) = (s, e)   : letVars vs
letVars (Sym s           : vs) = (s, Nil) : letVars vs
letVars []                     = []
letVars e                      = error $ "Malformed let var entry: " <> show e
