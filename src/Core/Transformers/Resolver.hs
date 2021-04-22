module Core.Transformers.Resolver
  ( resolveSymbols
  ) where

import Data.Map qualified as Map

import Core.AST qualified as A
import Core.IR
import Core.CodeGen.Primitives

type Res = (Text, Expr)

-- FIXME Pass stack allocation via arg, not state, to re-use slots in
-- independent subtrees; also add new functions to cleanly manipulate arg.
resolveSymbols :: [A.Expr] -> Either String [Expr]
resolveSymbols es = Right $ evalState (mapM (resolve prims) es) 0
  where
    prims = map (\s -> (s, Prim s)) $ Map.keys primitives

resolve :: [Res] -> A.Expr -> State Int Expr
resolve rs (A.Sym s) = pure $ resolution rs s

resolve rs (A.Let vs es) = do
  vars <- mapM (\b -> (A.bName b,) <$> allocVar) vs
  vs' <- zipWithM (\r@(_, Var i) b -> Binding i <$> resolve (r : rs) (A.bVal b)) vars vs
  Let vs' <$> mapM (resolve $ vars <> rs) es
  where
    allocVar = modify succ *> get <&> Var

resolve rs (A.LamDef (A.Args as) (A.FreeArgs fs) es) = do
  let enumWith c xs = zipWith (\x n -> (x, c n)) xs [1..]
      as'  = enumWith Arg as
      fs'  = enumWith CArg fs
      free = FreeArgs $ map (resolutionVar rs) fs
  LamDef (Args as) free
    <$> (withTemporaryState (length as)
         $ mapM (resolve $ as' <> fs' <> rs) es)

resolve rs (A.List es)  = List <$> mapM (resolve rs) es
resolve rs (A.If p t f) = If <$> resolve rs p <*> resolve rs t <*> resolve rs f

resolve _ (A.Lit x) = pure $ Lit x
resolve _ A.Nil     = pure Nil
resolve _ e         = error $ "Cannot resolve-transform " <> show e

resolution :: [Res] -> Text -> Expr
resolution rs s = case find ((== s) . fst) rs of
  Just (_, e) -> e
  _           -> error $ "failed to resolve " <> show s

resolutionVar :: [Res] -> Text -> Int
resolutionVar rs s = case resolution rs s of
  Var i -> i
  _     -> error $ "not a variable " <> show s

withTemporaryState :: s -> State s r -> State s r
withTemporaryState x f = get >>= \st -> put x *> f <* put st
