module Core.Transformers.Resolver
  ( resolveSymbols
  ) where

import Data.Map qualified as Map

import Core.AST
import Core.CodeGen.Primitives

type Res = (Text, Expr)

-- FIXME Pass stack allocation via arg, not state, to re-use slots in
-- independent subtrees; also add new functions to cleanly manipulate arg.
resolveSymbols :: [Expr] -> Either String [Expr]
resolveSymbols es = Right $ evalState (mapM (resolve prims) es) 0
  where
    prims = map (\s -> (s, Prim s)) $ Map.keys primitives

resolve :: [Res] -> Expr -> State Int Expr

resolve rs e@(Sym s) = case find ((==s) . fst) rs of
  Just (_, loc) -> pure loc
  Nothing       -> pure e

resolve rs (Let vs es) = do
  vars <- mapM (\b -> (name b,) <$> allocVar) vs
  vs' <- zipWithM (\r@(_, Var i) b -> Binding (Place i) <$> resolve (r : rs) (val b)) vars vs
  Let vs' <$> mapM (resolve $ vars <> rs) es
  where
    allocVar = modify succ *> get <&> Var
    name (Binding (Name s) _) = s
    name _                    = error "Binding already resolved"
    val (Binding _ v) = v

resolve rs (LamDef (Args as) (FreeArgs fs) es) = do
  let enumWith c xs = zipWith (\x n -> (x, c n)) xs [1..]
      as' = enumWith Arg as
      fs' = enumWith CArg $ map name fs
  free <- FreeArgs <$> mapM (resolveId rs) fs
  LamDef (Args as) free
    <$> (withTemporaryState (length as)
         $ mapM (resolve $ as' <> fs' <> rs) es)
  where
    name (Name s) = s
    name _        = error "Expected name"

    resolveId rs' (Name s) = case find ((==s) . fst) rs' of
      Just (_, Var i) -> pure $ Place i
      _               -> error "aiiee"
    resolveId _ _ = error "sigh"

resolve rs (List es)  = List <$> mapM (resolve rs) es
resolve rs (If p t f) = If <$> resolve rs p <*> resolve rs t <*> resolve rs f

resolve _ e = pure e

withTemporaryState :: s -> State s r -> State s r
withTemporaryState x f = get >>= \st -> put x *> f <* put st
