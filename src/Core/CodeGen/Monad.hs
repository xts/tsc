module Core.CodeGen.Monad
  ( CodeGen
  , Primitive
  , Var(..)
  , runCodeGen
  , context
  , lookupPrimitive
  , emit
  , addVariable
  , delVariable
  , lookupVariable
  , withStackSlot
  , allocStackSlot
  , freeStackSlot
  , stackSlot
  , closureSlot
  , stackSpace
  , funLabel
  ) where

import Control.Monad.RWS (RWST, execRWST, tell)
import Control.Monad.Except (Except, runExcept)
import Data.Map qualified as Map
import Prelude hiding (State)

import Core.AST

type Primitive = [Expr] -> CodeGen ()

data Env = Env
  { envContext    :: Text               -- ^ Current function.
  , envPrimitives :: Map Text Primitive -- ^ Available primitive functions.
  }

data Var
  = Stack Int
  | Closure Int

data Alloc = Alloc
  { stCurStack     :: Int           -- ^ Next free stack slot.
  , stMaxStack     :: Int           -- ^ Peak stack usage.
  , stNextLabel    :: Int           -- ^ Suffix of next local code label.
  , stVariables    :: [(Text, Var)] -- ^ Variable name -> stack index.
  }

type CodeGen a = RWST Env ByteString Alloc (Except String) a

runCodeGen
  :: Text
  -> Int
  -> Map Text Primitive
  -> CodeGen ()
  -> Either String (Int, ByteString)
runCodeGen ctx preallocStack ps f =
  runExcept $ first stackSpace <$> execRWST f env st
  where
    env = Env ctx ps
    st  = Alloc preallocStack preallocStack 0 []

emit :: ByteString -> CodeGen ()
emit = tell

context :: CodeGen Text
context = envContext <$> ask

lookupPrimitive :: Text -> CodeGen (Maybe Primitive)
lookupPrimitive name = Map.lookup name . envPrimitives <$> ask

addVariable :: Text -> Var -> CodeGen ()
addVariable name slot = do
  vars <- stVariables <$> get
  modify $ \st -> st { stVariables = (name, slot) : vars }

delVariable :: Text -> CodeGen ()
delVariable name = do
  vars <- stVariables <$> get
  let (before, _:after) = break ((== name) . fst) vars
  modify $ \st -> st { stVariables = before ++ after }

lookupVariable :: Text -> CodeGen (Maybe Var)
lookupVariable name = fmap snd . find ((== name) . fst) . stVariables <$> get

allocStackSlot :: CodeGen Int
allocStackSlot = do
  sp <- succ . stCurStack <$> get
  setStack sp
  pure sp

freeStackSlot :: CodeGen ()
freeStackSlot = do
  sp <- stCurStack <$> get
  setStack (pred sp)

withStackSlot :: (ByteString -> CodeGen a) -> CodeGen a
withStackSlot f = do
  sp <- allocStackSlot
  ret <- f (stackSlot sp)
  freeStackSlot
  pure ret

setStack :: Int -> CodeGen ()
setStack sp = do
  maxSp <- stMaxStack <$> get
  modify $ \st -> st { stCurStack = sp, stMaxStack = max maxSp sp }

stackSlot :: Int -> ByteString
stackSlot = fromString . show . ((-8) *)

closureSlot :: Int -> ByteString
closureSlot = fromString . show . (8 *)

stackSpace :: Alloc -> Int
stackSpace = to16s . (8*) . stMaxStack
  where
    to16s n = let r = n `rem` 16 in if r == 0 then n else n + 16 - r

funLabel :: CodeGen ByteString
funLabel = do
  ctx <- envContext <$> ask
  n <- stNextLabel <$> get
  modify $ \st -> st { stNextLabel = succ n }
  pure $ encodeUtf8 ctx <> "_" <> fromString (show n)
