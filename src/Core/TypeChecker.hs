module Core.TypeChecker
  ( typeCheck
  ) where

import Control.Monad.Except (runExcept)
import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Prelude hiding (Type, last)
import Relude.Unsafe (last)

import Core.AST
import Core.Transform (Transform, transform)
import Core.TypeChecker.Monad
import Core.TypeChecker.Types
import Core.TypeChecker.Variable

-- | Infer the type of an expression.
typeCheck :: Monad m => [Expr] -> Transform m [Expr]
typeCheck es = transform $ runExcept $ do
  evalStateT (mapM_ (infer env) es) 4
  pure es
  where
    env = Env (Map.fromList
               [ ("cons",    Scheme [0] $ TyFun [TyVar 0, TyList (TyVar 0)] (TyList (TyVar 0)))
               , ("display", Scheme [1] $ TyFun [TyVar 1] TyBool)
               , ("<",       Scheme []  $ TyFun [TyInt, TyInt] TyBool)
               , ("eq",      Scheme [2] $ TyFun [TyVar 2, TyVar 2] TyBool)
               , ("car",     Scheme [3] $ TyFun [TyList (TyVar 3)] (TyVar 3))
               ])

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
      (v, t) <- infer (apply u g') e
      let t' = generalise (apply v g') t
      pure (v <> u, assign n t' g')

infer g (LamDef (Args as) _ es) = do
  ts <- replicateM (length as) freshVar
  let g' = foldr (\(s, t) m -> assign s (Scheme [] t) m) g $ zip as ts
  (u, ss) <- inferSeq g' mempty es
  pure (u, apply u $ TyFun ts $ last ss)

infer g (List (e:es)) = do
  (u, t) <- infer g e
  (v, ts) <- inferSeq g u es
  f <- freshVar
  w <- unify (apply v t) (TyFun ts f)
  pure (w <> v <> u, apply w f)

infer g (If p bt bf) = do
  (u, _) <- infer g p
  (v, t) <- infer (apply u g) bt
  (w, s) <- infer (apply (v <> u) g) bf
  x <- unify (apply w t) s
  pure (x <> w <> v <> u, t)

infer _ (Lit (Bool _))   = pure (mempty, TyBool)
infer _ (Lit (Fixnum _)) = pure (mempty, TyInt)
infer _ (Lit (String _)) = pure (mempty, TyString)

infer _ e = error $ "can't infer " <> show e

inferSeq :: Env -> Subst -> [Expr] -> TC (Subst, [Type])
inferSeq g u es = second reverse <$> foldM go (u, []) es
  where
    go (u', ts) e' = do
      (v, t) <- infer (apply u' g) e'
      pure (v <> u', t:ts)

-- | Unify two types, finding a substition that transforms one to the other.
unify :: Type -> Type -> TC Subst
unify (TyVar n) s = bind n s
unify t (TyVar n) = bind n t
unify (TyFun as b) (TyFun cs d) | length as == length cs = do
  u <- foldM (\u (a, c) -> unify (apply u a) (apply u c)) mempty $ zip as cs
  mappend u <$> unify (apply u b) (apply u d)
unify (TyList (TyVar n)) (TyList t) = bind n t
unify (TyList t) (TyList (TyVar n)) = bind n t
unify s t | s == t    = pure mempty
          | otherwise = error $ "type mismatch: " <> show t <> " vs " <> show s

-- | Create a substitution from a type variable to a type.
bind :: Int -> Type -> TC Subst
bind n (TyVar k) | n == k = pure mempty
bind n t | n `Set.member` free t = error $ "error: occurs check failed for " <> show t
         | otherwise             = pure $ Subst $ Map.singleton n t
