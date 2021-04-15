module Core.Desugarer
  ( desugar
  ) where

import Core.Parser.AST

desugar :: [Expr] -> Either String [Expr]
desugar (FunDef name vs bs : es) = do
  bs' <- desugar bs
  es' <- desugar es
  pure [Let [(name, Lam vs bs')] es']
desugar (VarDef name e : es) = do
  [e'] <- desugar [e]
  es' <- desugar es
  pure [Let [(name, e')] es']
desugar (e : es) = (e:) <$> desugar es
desugar [] = pure []
