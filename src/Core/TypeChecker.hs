module Core.TypeChecker
  ( typeCheck
  , primitiveEnv
  , infer
  ) where

import Control.Monad.Except (runExcept)
import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Prelude hiding (Type, last)
import Relude.Unsafe (last)

import Core.AST
import Core.CodeGen.Monad (Primitive(..))
import Core.CodeGen.Primitives (primitives)
import Core.Transform (Transform, transform)
import Core.TypeChecker.Monad
import Core.TypeChecker.Types
import Core.TypeChecker.Variable

-- | Infer the type of an expression.
typeCheck :: Monad m => [Expr] -> Transform m [Expr]
typeCheck es = transform $ runExcept $ evalStateT go 0
  where
    go = do
      env <- primitiveEnv
      mapM_ (infer env) es
      pure es

-- | Environment pre-populated with primitive types.
primitiveEnv :: TC Env
primitiveEnv = foldM addPrimitive mempty $ Map.toList primitives
  where
    addPrimitive g (name, Primitive _ ty) = do
      ty' <- generalise g <$> instantiate (generalise g ty)
      pure $ assign name ty' g

-- | Instantiate a scheme with fresh variables.
instantiate :: Scheme -> TC Type
instantiate (Scheme vs t) = do
  apply <$> (Subst <$> foldM fresh mempty vs) <*> pure t
  where
    fresh s k = Map.insert k <$> freshVar <*> pure s

-- | Universally quantify a type over its free variables.
generalise :: Env -> Type -> Scheme
generalise env t = Scheme (Set.toList $ free t `Set.difference` free env) t

-- | Infer the type of an expression in the given environment.
infer :: Env -> Expr -> TC (Subst, Type)
infer _ Nil = (mempty,) . TyList <$> freshVar

infer g (Sym s) = case lookup s g of
  Just t  -> (mempty,) <$> instantiate t
  Nothing -> error $ "internal error: no such binding " <> show s

infer g (Let bs es) = do
  (u, g') <- foldM binding (mempty, g) bs
  (v, ts) <- inferSeq g' u es
  pure (u <> v, last ts)
  where
    binding (u, g') (Binding n e) = do
      -- Pre-bind to a fresh variable for recursive function bindings.
      f <- freshVar
      let g'' = assign n (Scheme [] f) g'
      -- Infer the type of the body and unify with the fresh variable.
      (v, t) <- infer (apply u g'') e
      w <- unify (apply v f) t
      -- Ugly and possibly incorrect; generalise the binder type.
      let g''' = assign n (generalise g'' (apply w t)) (apply (v <> w) g'')
      pure (u <> v <> w, g''')

infer g (LamDef (Args as) _ es) = do
  ts <- replicateM (length as) freshVar
  let g' = foldr (\(s, t) m -> assign s (Scheme [] t) m) g $ zip as ts
  (u, ss) <- inferSeq g' mempty es
  pure (u, apply u $ TyFun ts $ last ss)

infer g (List (e:es)) = do
  (u, t) <- infer g e
  (v, ts) <- inferSeq (apply u g) u es
  f <- freshVar
  w <- unify (apply v t) (TyFun ts f)
  pure (u <> v <> w, apply w f)

infer g (If p bt bf) = do
  (u, _) <- infer g p
  (v, t) <- infer (apply u g) bt
  (w, s) <- infer (apply (u <> v) g) bf
  x <- unify (apply w t) s
  pure (u <> v <> w <> x, apply (w <> x) t)

infer _ (Lit (Bool _))   = pure (mempty, TyBool)
infer _ (Lit (Fixnum _)) = pure (mempty, TyInt)
infer _ (Lit (Char _))   = pure (mempty, TyChar)
infer _ (Lit (String _)) = pure (mempty, TyString)

infer _ e = error $ "can't infer " <> show e

inferSeq :: Env -> Subst -> [Expr] -> TC (Subst, [Type])
inferSeq g u es = second reverse <$> foldM go (u, []) es
  where
    go (u', ts) e' = do
      (v, t) <- infer (apply u' g) e'
      pure (u' <> v, t:ts)

-- | Unify two types, finding a substition that transforms one to the other.
unify :: Type -> Type -> TC Subst
unify (TyVar n) s = bind n s
unify t (TyVar n) = bind n t

unify (TyList (TyVar n)) (TyList t) = bind n t
unify (TyList t) (TyList (TyVar n)) = bind n t

unify (TyFun as b) (TyFun cs d) | length as == length cs = do
  u <- foldM (\u (a, c) -> mappend u <$> unify (apply u a) (apply u c)) mempty $ zip as cs
  mappend u <$> unify (apply u b) (apply u d)

unify (TyFunV a b) t@(TyFun cs _) = unify (TyFun (replicate (length cs) a) b) t
unify t s@(TyFunV {}) = unify s t

unify TyBot _ = pure mempty
unify _ TyBot = pure mempty

unify s t | s == t    = pure mempty
          | otherwise = error $ "type mismatch: " <> show t <> " vs " <> show s

-- | Create a substitution from a type variable to a type.
bind :: Int -> Type -> TC Subst
bind n (TyVar k) | n == k = pure mempty
bind n t | n `Set.member` free t = error $ "error: occurs check failed for " <> show t
         | otherwise             = pure $ Subst $ Map.singleton n t
