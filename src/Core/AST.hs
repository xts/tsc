module Core.AST
    ( Expr(..)
    , Literal(..)
    , Label(..)
    , Name
    , Binding(..)
    , Args(..)
    , FreeArgs(..)
    , traverseAst
    , prettyPrint
    ) where

import Prelude hiding (intercalate)

import Core.PrettyPrinter

data Expr
  = Nil
  | Sym Text
  | Lit Literal
  | List [Expr]
  | Let [Binding] [Expr]
  | LamDef Args FreeArgs [Expr]
  | If Expr Expr Expr
  | FunDef Name Args [Expr]
  | VarDef Name Expr
  deriving (Eq, Show)

data Literal
  = Bool Bool
  | Char Char
  | Fixnum Int
  | String (Either Text Label)
  deriving (Eq, Show)

newtype Label = Label { unLabel :: Text }
  deriving (Eq, Show)

type Name = Text

data Binding = Binding { bName :: Text, bVal :: Expr }
  deriving (Eq, Show)

newtype Args = Args { unArgs :: [Text] }
  deriving (Eq, Show)

newtype FreeArgs = FreeArgs { unFreeArgs :: [Text] }
  deriving (Eq, Show)

traverseAst :: Monad m => (Expr -> m Expr) -> [Expr] -> m [Expr]
traverseAst h = mapM g
  where
    g e = go e >>= h
    go (List es)         = List <$> mapM g es
    go (Let bs es)       = Let <$> mapM (\(Binding s e) -> Binding s <$> g e) bs <*> mapM g es
    go (LamDef as fs es) = LamDef as fs <$> mapM g es
    go (If p t f)        = If <$> g p <*> g t <*> g f
    go (FunDef n as es)  = FunDef n as <$> mapM g es
    go (VarDef n e)      = VarDef n <$> g e
    go e                 = pure e

instance HasPrettyPrint [Expr] where
  prettyPrint = runPP . intercalate newline . map go
    where
      go Nil = "()"
      go (Sym s) = string s
      go (Lit (Bool True)) = "#t"
      go (Lit (Bool False)) = "#f"
      go (Lit (Fixnum k)) = show k
      go (Lit (Char c)) = show c
      go (Lit (String (Left s))) = show s
      go (Lit (String (Right (Label l)))) = "%L" .+ string l
      go (List es) = "(" .+ intercalate " " (map go es) .+ ")"
      go (VarDef s e) = "(define " .+ string s .+ " " .+ go e .+ ")"

      go (Let vs es) =
          "(let (" .+ align (map letBinding vs) .+ ")"
        .| "  " .+ align (map go es) .+ ")"
        where
          letBinding (Binding s e) = "(" .+ string s .+ " " .+ go e .+ ")"

      go (LamDef (Args as) (FreeArgs fs) es) =
          "(lambda (" .+ spaced string as .+ ")" .+ " [" .+ spaced string fs .+ "]"
        .| "  " .+ align (map go es) .+ ")"

      go (If p t f) =
          "(if " .+ go p
        .| "      " .+ align [go t, go f] .+ ")"

      go (FunDef s (Args as) es) =
          "(define " .+ string s .+ " (" .+ spaced string as .+ ")"
        .| "  " .+ align (map go es) .+ ")"
