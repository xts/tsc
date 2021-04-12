module Core.Renamer
  ( rename
  ) where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Data.Foldable (foldrM)
import Data.Map (Map, lookup, insert, insertWith)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Prelude hiding (lookup)

import Core.Parser.AST
import Core.CodeGen.Primitives

data Names = Names
  { names :: Map Text Text
  , counts :: Map Text Int
  } deriving (Show)

type Bindings = Map Text [Text]

type R a = StateT Names (Except String) a

rename :: [Expr] -> Either String [Expr]
rename es = runExcept $ evalStateT (mapM (expr mempty) es) $ Names mempty mempty

expr :: Bindings -> Expr -> R Expr
expr bs (Sym s)     = Sym <$> bound bs s
expr bs (Let vs es) = letForm bs vs es
expr bs (Lam ps es) = lambda bs ps es
expr bs (List es)   = List <$> mapM (expr bs) es
expr bs (If p t f)  = If <$> expr bs p <*> expr bs t <*> expr bs f
expr _  e           = pure e

letForm :: Bindings -> [(Text, Expr)] -> [Expr] -> R Expr
letForm bs vs es = do
  bs' <- foldrM bind bs $ map fst vs
  vs' <- zip <$> mapM (bound bs' . fst) vs <*> mapM (expr bs . snd) vs
  Let vs' <$> mapM (expr bs') es

lambda :: Bindings -> [Text] -> [Expr] -> R Expr
lambda bs ps es = do
  bs' <- foldrM bind bs ps
  Lam <$> mapM (bound bs') ps <*> mapM (expr bs') es

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
  let bs' = insertWith (<>) s [name] bs
  remember name s
  pure bs'

count :: Text -> R Int
count s = fromMaybe 0 . lookup s <$> gets counts

remember :: Text -> Text -> R ()
remember name base = do
  Names ns cs <- get
  n           <- succ <$> count base
  put $ Names (insert name base ns) (insert base n cs)
