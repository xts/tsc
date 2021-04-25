module Core.Transformers.Desugarer
  ( desugar
  ) where

import Core.AST
import Core.Transform (Transform, transform)

desugar :: Monad m => [Expr] -> Transform m [Expr]
desugar es = transform $ Right $ define es

define :: [Expr] -> [Expr]
define (FunDef name vs bs : es) =
  [Let [Binding name (LamDef vs (FreeArgs []) (define bs))] (define es)]
define (VarDef name e : es) = let [e'] = define [e] in
  [Let [Binding name e'] (define es)]
define (e : es) = e : define es
define [] = []
