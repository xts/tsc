module Core.TypeChecker.Types
  ( Type(..)
  , Id
  , Scheme(..)
  ) where

import Prelude hiding (Type)
import Text.Show qualified

-- | An unquantified type.
data Type
   = TyVar Id
   | TyFun [Type] Type
   | TyList Type
   | TyInt
   | TyBool
   | TyString
   deriving (Eq)

-- | Type variables are identified by a number.
type Id = Int

instance Show Type where
  show (TyVar n)   = "t" <> show n
  show (TyFun s t) = "(" <> (intercalate " " $ map show s) <> ") -> " <> show t
  show (TyList t)  = "[" <> show t <> "]"
  show TyInt       = "Int"
  show TyBool      = "Bool"
  show TyString    = "String"

-- | A scheme is a universally quantified type.
data Scheme = Scheme [Int] Type
  deriving (Show)
