module Core.IR
  ( Expr(..)
  , FreeArgs(..)
  , Binding(..)
  , Args(..)
  , Label(..)
  , Literal(..)
  , traverseIr
  ) where

import Core.AST (Args(..), Label(..), Literal(..))

data Expr
  = Nil
  | Arg Int
  | CArg Int
  | Prim Text
  | Var Int
  | Lit Literal
  | List [Expr]
  | Let [Binding] [Expr]
  | LamDef Args FreeArgs [Expr]
  | LamDec Args FreeArgs Label
  | If Expr Expr Expr
  deriving (Eq, Show)

newtype FreeArgs = FreeArgs { unfreeArgs :: [Int] }
  deriving (Eq, Show)

data Binding = Binding Int Expr
  deriving (Eq, Show)

traverseIr :: Monad m => (Expr -> m Expr) -> [Expr] -> m [Expr]
traverseIr h = mapM g
  where
    g e = go e >>= h
    go (List es)         = List <$> mapM g es
    go (Let bs es)       = Let <$> mapM (\(Binding i e) -> Binding i <$> g e) bs <*> mapM g es
    go (LamDef as fs es) = LamDef as fs <$> mapM g es
    go (If p t f)        = If <$> g p <*> g t <*> g f
    go e                 = pure e
