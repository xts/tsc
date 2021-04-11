module Core.CodeGen.State
  ( CodeGen
  , Primitive
  , State
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
  , stackSpace
  , funLabel
  ) where

import Control.Monad.RWS (RWST, execRWST, ask, tell, get, modify)
import Control.Monad.Except (Except, runExcept)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Core.Analyser.AST

type Primitive = [Expr] -> CodeGen ()

data Env = Env
  { envContext    :: Text               -- ^ Current function.
  , envPrimitives :: Map Text Primitive -- ^ Available primitive functions.
  }

data State = State
  { stCurStack     :: Int           -- ^ Next free stack slot.
  , stMaxStack     :: Int           -- ^ Peak stack usage.
  , stNextLabel    :: Int           -- ^ Suffix of next local code label.
  , stVariables    :: [(Text, Int)] -- ^ Variable name -> stack index.
  }

type CodeGen a = RWST Env ByteString State (Except String) a

runCodeGen :: Text -> Int -> Map Text Primitive -> CodeGen () -> Either String (State, ByteString)
runCodeGen ctx preallocStack ps f = runExcept $ execRWST f env state
  where
    env   = Env ctx ps
    state = State preallocStack preallocStack 0 []

emit :: ByteString -> CodeGen ()
emit = tell

context :: CodeGen Text
context = envContext <$> ask

lookupPrimitive :: Text -> CodeGen (Maybe Primitive)
lookupPrimitive name = Map.lookup name . envPrimitives <$> ask

addVariable :: Text -> Int -> CodeGen ()
addVariable name slot = do
  vars <- stVariables <$> get
  modify $ \st -> st { stVariables = (name, slot) : vars }

delVariable :: Text -> CodeGen ()
delVariable name = do
  vars <- stVariables <$> get
  let (before, after) = break ((== name) . fst) vars
  modify $ \st -> st { stVariables = before ++ tail after }

lookupVariable :: Text -> CodeGen (Maybe Int)
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

stackSpace :: State -> Int
stackSpace = to16s . (8*) . stMaxStack
  where
    to16s n = let r = n `rem` 16 in if r == 0 then n else n + 16 - r

funLabel :: CodeGen ByteString
funLabel = do
  ctx <- envContext <$> ask
  n <- stNextLabel <$> get
  modify $ \st -> st { stNextLabel = succ n }
  pure $ encodeUtf8 ctx <> "_" <> fromString (show n)
