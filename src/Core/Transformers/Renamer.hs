module Core.Transformers.Renamer
  ( renameSymbols
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Data.Foldable (foldrM)
import Data.Map (lookup, insert, insertWith)
import Data.Text (pack)

import Core.AST
import Core.CodeGen.Primitives
import Core.Transform (Transform, transform)

data Names = Names
  { names :: Map Text Text
  , counts :: Map Text Int
  } deriving (Show)

type Bindings = Map Text [Text]

type R a = StateT Names (Except String) a

renameSymbols :: Monad m => [Expr] -> Transform m [Expr]
renameSymbols es =
  transform $ runExcept $ evalStateT (mapM (expr mempty) es) $ Names mempty mempty

expr :: Bindings -> Expr -> R Expr
expr bs (Sym s)     = Sym <$> bound bs s
expr bs (Let vs es) = letForm bs vs es
expr bs (List es)   = List <$> mapM (expr bs) es
expr bs (If p t f)  = If <$> expr bs p <*> expr bs t <*> expr bs f
expr bs (LamDef ps _ es) = lambda bs ps es
expr _  e = pure e

letForm :: Bindings -> [Binding] -> [Expr] -> R Expr
letForm bs vs es = do
  bs' <- foldrM bind bs $ map bName vs
  vs' <- mapM (rebind bs') vs
  Let vs' <$> mapM (expr bs') es
  where
    rebind bs' (Binding s e) = do
      s' <- bound bs' s
      Binding s' <$> expr (insertWith (<>) s [s'] bs') e

lambda :: Bindings -> Args -> [Expr] -> R Expr
lambda bs (Args ps) es = do
  bs' <- foldrM bind bs ps
  LamDef <$> (Args <$> mapM (bound bs') ps) <*> pure (FreeArgs []) <*> mapM (expr bs') es

bound :: Bindings -> Text -> R Text
bound bs s = case lookup s bs of
  Just (t:_) -> pure t
  _          -> case lookup s primitives of
    Just _ -> pure s
    _      -> throwError $ "No such binding: " <> show s

bind :: Text -> Bindings -> R Bindings
bind s bs = do
  n <- count s
  let name = s <> pack (show n)
  remember name s
  pure $ insertWith (<>) s [name] bs

count :: Text -> R Int
count s = fromMaybe 0 . lookup s <$> gets counts

remember :: Text -> Text -> R ()
remember name base = do
  Names ns cs <- get
  n           <- succ <$> count base
  put $ Names (insert name base ns) (insert base n cs)
