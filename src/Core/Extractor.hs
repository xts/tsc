module Core.Extractor
  ( Function(..)
  , extractLambdas
  , extractStrings
  ) where

import Core.AST
import Data.Map qualified as Map

data Function = Function
  { funLabel :: Label
  , funArgs :: Args
  , funBody :: [Expr]
  } deriving (Eq, Show)

extractLambdas :: [Expr] -> ([Expr], [Function])
extractLambdas es = runState (traverseAst go es) []
  where
    go :: Expr -> State [Function] Expr
    go (LamDef as (Just fs) es') = do
      labels <- get
      let lab = Label $ "_lambda_" <> show (length labels)
      modify (Function lab as es' :)
      pure $ LamDec as fs lab
    go e = pure e

extractStrings :: [Expr] -> ([Expr], Map Text Label)
extractStrings es = runState (traverseAst go es) mempty
  where
    go :: Expr -> State (Map Text Label) Expr
    go (Lit (String (Left s))) = Lit . String . Right <$> label s
    go e                       = pure e

    label :: Text -> State (Map Text Label) Label
    label s = get >>= \labels -> case Map.lookup s labels of
      Just l  -> pure l
      Nothing -> do
        let lab = Label $ "_string_" <> show (Map.size labels)
        modify $ Map.insert s lab
        pure lab
