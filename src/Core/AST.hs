module Core.AST
    ( Expr(..)
    , Literal(..)
    , Label(..)
    , Name(..)
    , Binding(..)
    , Args(..)
    , traverseAst
    ) where

-- TODO Parameterise on expr / label
data Expr
  = Nil
  | Arg Int
  | CArg Int
  | Prim Text
  | Sym Text
  | Lit Literal
  | List [Expr]
  | Let [Binding] [Expr]
  | LamDef Args (Maybe Args) [Expr]
  | LamDec Args Args Label
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

newtype Name = Name { unName :: Text }
  deriving (Eq, Show)

data Binding = Binding { bName :: Text, bVal :: Expr }
  deriving (Eq, Show)

newtype Args = Args { unArgs :: [Text] }
  deriving (Eq, Show)

traverseAst :: Monad m => (Expr -> m Expr) -> [Expr] -> m [Expr]
traverseAst h = mapM g
  where
    g e = go e >>= h
    go (List es)         = List <$> mapM g es
    go (Let bs es)       = Let <$> mapM (\(Binding n e) -> Binding n <$> g e) bs <*> mapM g es
    go (LamDef as fs es) = LamDef as fs <$> mapM g es
    go (If p t f)        = If <$> g p <*> g t <*> g f
    go (FunDef n as es)  = FunDef n as <$> mapM g es
    go (VarDef n e)      = VarDef n <$> g e
    go e                 = pure e
