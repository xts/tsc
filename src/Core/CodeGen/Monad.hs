module Core.CodeGen.Monad
  ( CodeGen
  , Arity(..)
  , Primitive(..)
  , runCodeGen
  , contextName
  , reservedWords
  , indentColumn
  , lookupPrimitive
  , primitive
  , arity
  , emit
  , funLabel
  ) where

import Control.Lens
import Control.Monad.RWS (RWST, evalRWST, tell)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Map qualified as Map
import Prelude hiding (State, Type)

import Core.IR
import Core.TypeChecker.Types

type CodeGen a = RWST Primitives ByteString GenState (Except String) a

data Arity = Arity Int | Indefinite
  deriving (Eq, Show)

data Primitive = Primitive
  { pEmitter :: [Expr] -> CodeGen ()
  , pType    :: Type
  }

newtype Primitives = Primitives { unPrimitives :: Map Text Primitive }

data GenState = GenState
  { _contextName   :: Text  -- ^ Current function.
  , _reservedWords :: Int   -- ^ Reserved stack words in the current function.
  , _nextLabel     :: Int   -- ^ Suffix of next local code label.
  , _indentColumn  :: Int   -- ^ Assembly indentation level for debugging.
  }
makeLenses ''GenState

runCodeGen :: Map Text Primitive -> CodeGen () -> Either String ByteString
runCodeGen ps f = runExcept $ snd <$> evalRWST f (Primitives ps) st
  where
    st  = GenState mempty 0 0 4

emit :: ByteString -> CodeGen ()
emit = tell

lookupPrimitive :: Text -> CodeGen (Maybe Primitive)
lookupPrimitive name = Map.lookup name . unPrimitives <$> ask

primitive :: Text -> CodeGen Primitive
primitive name = lookupPrimitive name >>= \case
  Just prim -> pure prim
  Nothing   -> throwError $ "internal error: lost primitive " <> show name

arity :: Primitive -> Arity
arity (Primitive _ (TyFun as _)) = Arity $ length as
arity (Primitive _ (TyFunV _ _)) = Indefinite
arity _ = error "internal error: primitive is not a function"

funLabel :: CodeGen ByteString
funLabel = do
  ctx <- use contextName
  n <- nextLabel <<%= succ
  pure $ encodeUtf8 ctx <> "_" <> fromString (show n)
