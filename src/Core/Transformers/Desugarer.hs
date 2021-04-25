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
define (VarDef name e : es) =
  [Let [Binding name $ define1 e] (define es)]
define (Let vs bs : es) =
  Let (map (\(Binding n e) -> Binding n (define1 e)) vs) (define bs) : define es
define (If p t f : es) =
  If (define1 p) (define1 t) (define1 f) : define es
define (List es : es') = List (define es) : define es'
define (e : es) = e : define es
define [] = []

define1 :: Expr -> Expr
define1 e = case define [e] of
  [e'] -> e'
  _    -> error "internal error: define1"
