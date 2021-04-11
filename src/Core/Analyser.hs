module Core.Analyser
  ( Info(..)
  , Label(..)
  , Lambda(..)
  , analyse
  ) where

import Control.Monad.State (State, runState, get, modify)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)

import Core.Analyser.AST
import Core.Parser.AST qualified as P

data Lambda = Lambda
  { lmParams :: [Text]
  , lmBody   :: [Expr]
  } deriving (Eq, Show)

data Info = Info
  { inStrings :: Map Text Label
  , inLambdas :: [(Lambda, Label)]
  } deriving (Show)

type Analyser a = State Info a

analyse :: [P.Expr] -> ([Expr], Info)
analyse es = runState (mapM expr es) $ Info Map.empty []

expr :: P.Expr -> Analyser Expr
expr P.Nil       = pure Nil
expr (P.Sym s)   = pure $ Sym s
expr (P.List (P.Sym "lambda" : ps : es)) = do
  params <- expr ps
  body   <- mapM expr es
  Lam <$> lambdaLabel params body
expr (P.List xs) = List <$> mapM expr xs
expr (P.Lit lit) = literal lit

literal :: P.Literal -> Analyser Expr
literal (P.Bool b)   = pure $ Lit $ Bool b
literal (P.Char c)   = pure $ Lit $ Char c
literal (P.Fixnum k) = pure $ Lit $ Fixnum k
literal (P.String s) = Lit . String <$> stringLabel s

indexArgs :: [Text] -> [Expr] -> [Expr]
indexArgs vs es = map (mapExpr go) es
  where
    go e@(Sym s) = case index' s of
      Just i  -> Arg i
      Nothing -> e
    go e = e
    index' s = fmap snd $ find ((== s) . fst) (zip vs [0..])

lambdaLabel :: Expr -> [Expr] -> Analyser Label
lambdaLabel params body = do
  let ps     = map sym $ toList params
      bd     = indexArgs ps body
      lambda = Lambda ps bd
  lambdas <- inLambdas <$> get
  let lb = label "_lambda_" (length lambdas)
  modify $ \st -> st { inLambdas = (lambda, lb):lambdas }
  pure lb

stringLabel :: Text -> Analyser Label
stringLabel text = do
  strings <- inStrings <$> get
  case Map.lookup text strings of
    Just lb -> pure lb
    Nothing -> do
      let lb = label "_string_" (Map.size strings)
      modify $ \st -> st { inStrings = Map.insert text lb strings }
      pure lb

label :: Text -> Int -> Label
label prefix n = Label $ prefix <> pack (show n)
