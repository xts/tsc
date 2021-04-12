module Core.Analyser
  ( Info(..)
  , Label(..)
  , Lambda(..)
  , analyse
  ) where

import Control.Monad.State (State, runState,gets, modify)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)

import Core.Analyser.AST
import Core.Parser.AST qualified as P

type Bindings = Set Text

data Lambda = Lambda
  { lmParams :: [Text]
  , lmFree   :: [Text]
  , lmBody   :: [Expr]
  } deriving (Eq, Show)

data Info = Info
  { inStrings :: Map Text Label
  , inLambdas :: [(Lambda, Label)]
  } deriving (Show)

type Analyser a = State Info a

analyse :: [P.Expr] -> ([Expr], Info)
analyse es = runState (mapM (expr mempty) es) $ Info Map.empty []

expr :: Bindings -> P.Expr -> Analyser Expr
expr _  P.Nil         = pure Nil
expr _  (P.Lit lit)   = literal lit
expr _  (P.Sym s)     = pure $ Sym s
expr bs (P.List xs)   = List <$> mapM (expr bs) xs
expr bs (P.Let vs es) = letForm bs vs es
expr bs (P.Lam ps es) = lambda bs ps es
expr bs (P.If p t f)  = If <$> expr bs p <*> expr bs t <*> expr bs f

literal :: P.Literal -> Analyser Expr
literal (P.Bool b)   = pure $ Lit $ Bool b
literal (P.Char c)   = pure $ Lit $ Char c
literal (P.Fixnum k) = pure $ Lit $ Fixnum k
literal (P.String s) = Lit . String <$> stringLabel s

letForm :: Bindings -> [(Text, P.Expr)] -> [P.Expr] -> Analyser Expr
letForm bs vs es = do
  let bs' = Set.fromList (map fst vs) <> bs
  vs' <- mapM (letParam bs) vs
  es' <- mapM (expr bs') es
  pure $ Let vs' es'

letParam :: Bindings -> (Text, P.Expr) -> Analyser (Text, Expr)
letParam bs (s, e) = (s,) <$> expr (Set.insert s bs) e

indexArgs :: [Text] -> [Expr] -> [Expr]
indexArgs args = map (mapExpr go)
  where
    go e@(Sym s) = maybe e Arg (index' s)
    go e = e
    index' s = snd <$> find ((== s) . fst) (zip args [1..])

freeArgs :: Bindings -> [Expr] -> Set Text
freeArgs bs = mconcat . map free
  where
    free (Sym s) | s `elem` bs = Set.singleton s
                 | otherwise   = mempty
    free (Let vs es) =
      let vals = freeArgs bs $ map snd vs
          body = freeArgs (bs `Set.difference` Set.fromList (map fst vs)) es
      in vals <> body
    free (List es) = freeArgs bs es
    free _         = mempty

lambda :: Bindings -> [Text] -> [P.Expr] -> Analyser Expr
lambda bs ps es = do
  body    <- indexArgs ps <$> mapM (expr bs) es
  lambdas <- gets inLambdas
  let free = Set.toList $ freeArgs bs body
  let lb = label "_lambda_" (length lambdas)
  modify $ \st -> st { inLambdas = (Lambda ps free body, lb) : lambdas }
  pure $ Lam lb

stringLabel :: Text -> Analyser Label
stringLabel text = do
  strings <- gets inStrings
  case Map.lookup text strings of
    Just lb -> pure lb
    Nothing -> do
      let lb = label "_string_" (Map.size strings)
      modify $ \st -> st { inStrings = Map.insert text lb strings }
      pure lb

label :: Text -> Int -> Label
label prefix n = Label $ prefix <> pack (show n)
