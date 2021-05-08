module Core.TypeChecker.Variable
  ( Variable(..)
  , Subst(..)
  ) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Prelude hiding (Type)

import Core.TypeChecker.Types

-- | Some data types contain variables that can be substituted and collected.
class Variable a where
  apply :: Subst -> a -> a
  free :: a -> Set Int

instance Variable Type where
  apply (Subst s) t@(TyVar n) = case Map.lookup n s of
    Just t' -> t'
    Nothing -> t
  apply s (TyFun t r) = TyFun (map (apply s) t) (apply s r)
  apply s (TyList t)  = TyList (apply s t)
  apply _ t           = t

  free (TyVar n)   = Set.singleton n
  free (TyFun s t) = free t <> mconcat (map free s)
  free (TyList t)  = free t
  free _           = mempty

-- | A substitution is a mapping from identifiers to types.
newtype Subst = Subst (Map Int Type)
  deriving (Monoid, Show)

instance Semigroup Subst where
  a <> (Subst b) = Subst $ Map.map (apply a) b `Map.union` b

instance Variable Scheme where
  apply u (Scheme ns t) = Scheme ns $ apply (u `without` ns) t
  free (Scheme ns t) = free t `Set.difference` Set.fromList ns

-- | Restrict a substitution set.
without :: Subst -> [Int] -> Subst
without (Subst u) ns = Subst $ Map.filterWithKey (\k _ -> not (k `elem` ns)) u
