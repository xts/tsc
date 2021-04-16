module Core.Extractor
  ( extractLambdas
  , extractStrings
  ) where

import Core.AST
import Data.Map qualified as Map

extractLambdas :: [Expr] -> ([Expr], [(Label, Int, [Expr])])
extractLambdas es = runState (traverseAst go es) []
  where
    go :: Expr -> State [(Label, Int, [Expr])] Expr
    go (LamDef (Args as) (Just (Args fs)) es') = do
      labels <- get
      let lab = Label $ "_lambda_" <> show (length labels)
      modify ((lab, length as, es'):)
      pure $ LamDec (Args as) (Args fs) lab
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
