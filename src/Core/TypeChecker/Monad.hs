module Core.TypeChecker.Monad
  ( Env(..)
  , TC
  , assign
  , lookup
  , freshVar
  ) where

import Control.Monad.Except (Except)
import Data.Map qualified as Map
import Prelude hiding (Type)

import Core.TypeChecker.Types
import Core.TypeChecker.Variable

-- | Our type checker monad is a simple state monad holding the next type variable id.
type TC a = StateT Id (Except String) a

-- | Our environment is a mapping from symbols to types.
newtype Env = Env (Map Text Scheme)
  deriving (Monoid, Show)

deriving instance Semigroup Env

instance Variable Env where
  apply u (Env g) = Env $ Map.map (apply u) g
  free (Env g) = Map.foldr ((<>) . free) mempty g

assign :: Text -> Scheme -> Env -> Env
assign s t (Env g) = Env $ Map.insert s t g

lookup :: Text -> Env -> Maybe Scheme
lookup s (Env g) = Map.lookup s g

-- | Allocate a fresh type variable.
freshVar :: TC Type
freshVar = TyVar <$> get <* modify succ
