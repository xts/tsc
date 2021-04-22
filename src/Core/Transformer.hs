module Core.Transformer
  ( transform
  ) where

import Core.AST qualified as A
import Core.IR
import Core.Transformers.Desugarer
import Core.Transformers.FreeFinder
import Core.Transformers.Renamer
import Core.Transformers.Resolver

transform :: [A.Expr] -> Either String [Expr]
transform es = desugar es >>= rename >>= findFree >>= resolveSymbols
