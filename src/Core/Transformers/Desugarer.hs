module Core.Transformers.Desugarer
  ( desugar
  ) where

import Core.AST

desugar :: [Expr] -> Either String [Expr]
desugar es = Right $ define es

define :: [Expr] -> [Expr]
define (FunDef name vs bs : es) =
  [Let [Binding name (LamDef vs (FreeArgs []) (define bs))] (define es)]
define (VarDef name e : es) = let [e'] = define [e] in
  [Let [Binding name e'] (define es)]
define (e : es) = e : define es
define [] = []
