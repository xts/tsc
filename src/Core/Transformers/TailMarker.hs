module Core.Transformers.TailMarker
  ( markTails
  ) where

import Core.IR
import Core.Transform (Transform, transform)

-- | Find applications in tail-call position and mark them as such.
markTails :: Monad m => [Expr] -> Transform m [Expr]
markTails = transform . Right . runIdentity . transformIr go
  where
    go (LamDef as fs es) = pure $ LamDef as fs $ markLast es
    go e                 = pure e

-- | Mark tail calls in the last expression of the list.
markLast :: [Expr] -> [Expr]
markLast (e1 : e2 : es) = e1 : markLast (e2 : es)
markLast [e]            = [mark e]
markLast []             = []

-- | Find and mark tail calls in the current expression.
mark :: Expr -> Expr
mark (App es)    = TApp es
mark (Let bs es) = Let bs $ markLast es
mark (If p t f)  = If p (mark t) (mark f)
mark e           = e
