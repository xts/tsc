module Core.Transformers.Lifter
  ( liftPrimitives
  ) where

import Data.Map qualified as Map

import Core.IR
import Core.CodeGen.Primitives
import Core.CodeGen.Monad (Primitive(..), Arity(..))
import Core.Transform (Transform, transform)

-- | Lift primitives to closures where necessary.
--
-- E.g. in the expression (map car xs), map must be supplied with
-- a closure, but car is a primitive. To solve this problem, we
-- translate (map car xs) into (map (lambda (x) (car x)) xs).
liftPrimitives :: Monad m => [Expr] -> Transform m [Expr]
liftPrimitives = transform . Right . map go
  where
    -- Leave primitives in operator position, these are good.
    go (List (Prim s : es)) = List (Prim s : map go es)

    -- Lift primitives in operand position.
    go (Prim s) = LamDef params (FreeArgs []) [List $ Prim s : args]
      where
        params = Args $ map (("x" <>) . show) [1..arity s]
        args   = map Arg [1..arity s]

    -- Recurse.
    go (If p t f)        = If (go p) (go t) (go f)
    go (List es)         = List $ map go es
    go (Let vs es)       = Let (map (\(Binding s e) -> Binding s $ go e) vs) $ map go es
    go (LamDef as fs bs) = LamDef as fs $ map go bs
    go e                 = e

arity :: Text -> Int
arity name = case Map.lookup name primitives of
  Just (Primitive _ (Arity n)) -> n
  Just (Primitive _ Indefinite) ->
    error $ "internal error: can't lift primitive with indefinite arity: " <> name
  Nothing ->
    error $ "internal error: no such primitive " <> name
