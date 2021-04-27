module Core.CodeGen.Monad
  ( CodeGen
  , Arity(..)
  , Primitive(..)
  , runCodeGen
  , context
  , setContext
  , reserved
  , setReserved
  , lookupPrimitive
  , pushIndent
  , popIndent
  , getIndent
  , primitive
  , emit
  , funLabel
  ) where

import Control.Monad.RWS (RWST, evalRWST, tell)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Map qualified as Map
import Prelude hiding (State)

import Core.IR

data Arity = Arity Int | Indefinite

data Primitive = Primitive
  { pEmitter :: [Expr] -> CodeGen ()
  , pArity   :: Arity
  }

newtype Primitives = Primitives { unPrimitives :: Map Text Primitive }

data GenState = GenState
  { stContext   :: Text  -- ^ Current function.
  , stReserved  :: Int   -- ^ Reserved stack words in the current function.
  , stNextLabel :: Int   -- ^ Suffix of next local code label.
  , stIndent    :: Int   -- ^ Assembly indentation level for debugging.
  }

type CodeGen a = RWST Primitives ByteString GenState (Except String) a

runCodeGen :: Map Text Primitive -> CodeGen () -> Either String ByteString
runCodeGen ps f = runExcept $ snd <$> evalRWST f (Primitives ps) st
  where
    st  = GenState mempty 0 0 4

emit :: ByteString -> CodeGen ()
emit = tell

context :: CodeGen Text
context = gets stContext

setContext :: Text -> CodeGen ()
setContext ctx = modify $ \st -> st { stContext = ctx }

reserved :: CodeGen Int
reserved = gets stReserved

setReserved :: Int -> CodeGen ()
setReserved k = modify $ \st -> st { stReserved = k }

lookupPrimitive :: Text -> CodeGen (Maybe Primitive)
lookupPrimitive name = Map.lookup name . unPrimitives <$> ask

pushIndent :: CodeGen ()
pushIndent = modify $ \st -> st { stIndent = stIndent st + 2 }

popIndent :: CodeGen ()
popIndent = modify $ \st -> st { stIndent = stIndent st - 2 }

getIndent :: CodeGen Int
getIndent = gets stIndent

primitive :: Text -> CodeGen Primitive
primitive name = lookupPrimitive name >>= \case
  Just prim -> pure prim
  Nothing   -> throwError $ "Internal error; lost primitive " <> show name

funLabel :: CodeGen ByteString
funLabel = do
  ctx <- context
  n <- stNextLabel <$> get
  modify $ \st -> st { stNextLabel = succ n }
  pure $ encodeUtf8 ctx <> "_" <> fromString (show n)
