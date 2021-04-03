module Core.AST
    ( AST(..)
    , Literal(..)
    ) where

import Data.Text (Text)

data AST = Lit Literal
  deriving (Eq, Show)

data Literal =
    Fixnum Int
  | String Text
  | Char Char
  | Bool Bool
  | Nil
  deriving (Eq, Show)
