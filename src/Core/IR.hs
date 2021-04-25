module Core.IR
  ( Expr(..)
  , FreeArgs(..)
  , Binding(..)
  , Args(..)
  , Label(..)
  , Literal(..)
  , transformIr
  , prettyPrint
  ) where

import Prelude hiding (intercalate)

import Core.AST (Args(..), Label(..), Literal(..))
import Core.PrettyPrinter

-- | Intermediate representation where all text symbols have been resolved to
-- arguments, closure arguments, indices, primitives, and values.
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

newtype FreeArgs = FreeArgs { unfreeArgs :: [Expr] }
  deriving (Eq, Show)

data Binding = Binding Int Expr
  deriving (Eq, Show)

-- | Perform a depth first transformation of the IR, applying the given function
-- at each node.
transformIr :: Monad m => (Expr -> m Expr) -> [Expr] -> m [Expr]
transformIr h = mapM g
  where
    g e = go e >>= h
    go (List es)         = List <$> mapM g es
    go (Let bs es)       = Let <$> mapM (\(Binding i e) -> Binding i <$> g e) bs <*> mapM g es
    go (LamDef as fs es) = LamDef as fs <$> mapM g es
    go (If p t f)        = If <$> g p <*> g t <*> g f
    go e                 = pure e

instance HasPrettyPrint [Expr] where
  prettyPrint = runPP . intercalate newline . map go
    where
      go :: Expr -> PP
      go Nil = "()"
      go (Arg i) = "%A" .+ show i
      go (CArg i) = "%C" .+ show i
      go (Prim p) = string p
      go (Var i) = "%V" .+ show i
      go (Lit (Bool True)) = "#t"
      go (Lit (Bool False)) = "#f"
      go (Lit (Fixnum k)) = show k
      go (Lit (Char c)) = show c
      go (Lit (String (Left s))) = show s
      go (Lit (String (Right (Label l)))) = "%L" .+ string l
      go (List es) = "(" .+ intercalate " " (map go es) .+ ")"

      go (Let vs es) =
          "(let (" .+ align (map letBinding vs) .+ ")"
        .| "  " .+ align (map go es) .+ ")"
        where
          letBinding (Binding i e) = "(" .+ go (Var i) .+ " " .+ go e .+ ")"

      go (LamDef (Args as) (FreeArgs fs) es) =
          "(lambda (" .+ spaced string as .+ ")" .+ " [" .+ spaced go fs .+ "]"
        .| "  " .+ align (map go es) .+ ")"

      go (LamDec (Args as) _ (Label l)) =
        "(lambda (" .+ spaced string as .+ ") %L" .+ show l .+ ")"

      go (If p t f) =
          "(if " .+ go p
        .| "      " .+ align [go t, go f] .+ ")"
