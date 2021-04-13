module Core.Analyser.AST
  ( Expr(..)
  , Literal(..)
  , Label(..)
  , Args(..)
  , FreeArgs(..)
  , toList
  , sym
  , mapExpr
  ) where

data Expr
  = Nil
  | Sym Text
  | Arg Int
  | Lit Literal
  | List [Expr]
  | Let [(Text, Expr)] [Expr]
  | Lam Args FreeArgs Label
  | If Expr Expr Expr
  deriving (Eq, Show)

data Literal
  = Fixnum Int
  | String Label
  | Char Char
  | Bool Bool
  deriving (Eq, Show)

newtype Label = Label { unLabel :: Text }
  deriving (Eq, Show)

newtype FreeArgs = FreeArgs { unFreeArgs :: [Text] }
  deriving (Eq, Show)

newtype Args = Args { unArgs :: [Text] }
  deriving (Eq, Show)

sym :: Expr -> Text
sym (Sym s) = s
sym e       = error $ "Not a symbol: " <> show e

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr g (List es)   = List $ map (mapExpr g) es
mapExpr g (Let vs es) = Let (map (second (mapExpr g)) vs) (map (mapExpr g) es)
mapExpr g (If p t f)  = If (mapExpr g p) (mapExpr g t) (mapExpr g f)
mapExpr g e           = g e
