module Core.TypeChecker.Types
  ( Type(..)
  , Id
  , Scheme(..)
  ) where

import Prelude hiding (Type)
import Text.Show qualified

-- | An unquantified type.
data Type
   = TyVar Id           -- ^ Type variable, e.g. a.
   | TyFun [Type] Type  -- ^ Function of finite arity, e.g. (a, b, c) -> d.
   | TyFunV Type Type   -- ^ Function of indefinite arity, e.g. + has (Int ...) -> Int.
   | TyList Type        -- ^ Homogeneous list.
   | TyInt              -- ^ Integer.
   | TyBool             -- ^ Boolean.
   | TyString           -- ^ String.
   | TyChar             -- ^ Character.
   deriving (Eq)

-- | Type variables are identified by a number.
type Id = Int

instance Show Type where
  show (TyVar n)    = "t" <> show n
  show (TyFun s t)  = "(" <> (intercalate " " $ map show s) <> ") -> " <> show t
  show (TyFunV s t) = "(" <> show s <> "...) -> " <> show t
  show (TyList t)   = "[" <> show t <> "]"
  show TyInt        = "Int"
  show TyBool       = "Bool"
  show TyString     = "String"
  show TyChar       = "Char"

-- | A scheme is a universally quantified type.
data Scheme = Scheme [Int] Type
  deriving (Show)
