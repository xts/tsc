module Core.CodeGen.Monad
  ( CodeGen
  , Primitive
  , runCodeGen
  , context
  , setContext
  , lookupPrimitive
  , emit
  , funLabel
  ) where

import Control.Monad.RWS (RWST, evalRWST, tell)
import Control.Monad.Except (Except, runExcept)
import Data.Map qualified as Map
import Prelude hiding (State)

import Core.AST

type Primitive = [Expr] -> CodeGen ()
newtype Primitives = Primitives { unPrimitives :: Map Text Primitive }

data GenState = GenState
  { stContext   :: Text  -- ^ Current function.
  , stNextLabel :: Int   -- ^ Suffix of next local code label.
  }

type CodeGen a = RWST Primitives ByteString GenState (Except String) a

runCodeGen :: Map Text Primitive -> CodeGen () -> Either String ByteString
runCodeGen ps f = runExcept $ snd <$> evalRWST f (Primitives ps) st
  where
    st  = GenState mempty 0

emit :: ByteString -> CodeGen ()
emit = tell

context :: CodeGen Text
context = gets stContext

setContext :: Text -> CodeGen ()
setContext ctx = modify $ \st -> st { stContext = ctx }

lookupPrimitive :: Text -> CodeGen (Maybe Primitive)
lookupPrimitive name = Map.lookup name . unPrimitives <$> ask

funLabel :: CodeGen ByteString
funLabel = do
  ctx <- context
  n <- stNextLabel <$> get
  modify $ \st -> st { stNextLabel = succ n }
  pure $ encodeUtf8 ctx <> "_" <> fromString (show n)
