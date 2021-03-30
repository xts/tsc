module AST
    ( AST(..)
    , Value(..)
    ) where

data AST = AEmpty | AValue Value
  deriving (Eq, Show)

data Value =
    VFixnum Int
  | VChar Char
  | VBool Bool
  | VNil ()
  deriving (Eq, Show)
