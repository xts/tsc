module Core.CodeGen
  ( lower
  ) where

import Control.Monad (forM_, when)
import Control.Monad.RWS (RWST, execRWST, ask, tell, get, modify)
import Control.Monad.Except (Except, runExcept, throwError)

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Char (isAscii, ord)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Core.AST

data Env = Env
  { _envFunctions :: Map Text Text -- ^ Function name -> function label.
  , _envContext   :: Text          -- ^ Current function.
  }

data Allocations = Allocations
  { stCurStack     :: Int
  , stMaxStack     :: Int
  , _stStringLabels :: Map Text Text
  , _stNextLabel    :: Int
  }

type CodeGen a = RWST Env ByteString Allocations (Except String) a

runCodeGen :: Text -> CodeGen () -> Either String (Allocations, ByteString)
runCodeGen ctx f = runExcept $ execRWST f env state
  where
    env   = Env Map.empty ctx
    state = Allocations 0 0 Map.empty 0

emit :: ByteString -> CodeGen ()
emit = tell

withStackSlot :: (ByteString -> CodeGen a) -> CodeGen a
withStackSlot f = do
  sp <- stCurStack <$> get
  setStack (succ sp)
  ret <- f (stackSlot sp)
  setStack sp
  pure ret

setStack :: Int -> CodeGen ()
setStack sp = do
  maxSp <- stMaxStack <$> get
  modify $ \st -> st { stCurStack = sp, stMaxStack = max maxSp sp }

stackSlot :: Int -> ByteString
stackSlot = fromString . show . ((-8) *) . succ

stackSpace :: Allocations -> Int
stackSpace = to16s . (8*) . stMaxStack
  where
    to16s n = let r = n `rem` 16 in if r == 0 then n else n + 16 - r

stringLabel :: Text -> CodeGen Text
stringLabel text = do
  labels <- _stStringLabels <$> get
  case Map.lookup text labels of
    Just lab -> pure lab
    Nothing    -> do
      let lab = "_string_" <> pack (show $ Map.size labels)
      modify $ \st -> st { _stStringLabels = Map.insert text lab labels }
      pure lab

funLabel :: CodeGen ByteString
funLabel = do
  context <- _envContext <$> ask
  n <- _stNextLabel <$> get
  modify $ \st -> st { _stNextLabel = succ n }
  pure $ encodeUtf8 context <> "_" <> fromString (show n)

strings :: Map Text Text -> CodeGen ()
strings labels = do
  forM_ (Map.toList labels) $ \(k, v) -> do
    sep
    dir $ "globl " <> encodeUtf8 v
    dir "p2align 4, 0x90"
    label $ encodeUtf8 v
    dir $ "asciz \"" <> encodeUtf8 k <> "\""

ins :: ByteString -> CodeGen ()
ins text = emit $ indent <> text <> "\n"

sep :: CodeGen ()
sep = emit "\n"

dir :: ByteString -> CodeGen ()
dir name = emit $ indent <> "." <> name <> "\n"

label :: ByteString -> CodeGen ()
label name = emit $ name <> ":\n"

indent :: ByteString
indent = "    "

lower :: Expr -> Either String ByteString
lower e = do
  (alloc, code)   <- function "_main" e
  (_, stringData) <- runCodeGen mempty $ strings $ _stStringLabels alloc
  pure $ code <> stringData

function :: Text -> Expr -> Either String (Allocations, ByteString)
function name e = do
  (alloc, body) <- runCodeGen name $ expr e
  (_, func) <- runCodeGen name $ do
    prologue (encodeUtf8 name) (stackSpace alloc)
    emit body
    ins "xorq %rax, %rax"
    epilogue (stackSpace alloc)
  pure (alloc, func)

expr :: Expr -> CodeGen ()
expr Nil           = ins "movq $0x3f, %rax"
expr (Lit lit)     = literal lit
expr (List (x:xs)) = call x xs
expr e             = throwError $ "Unable to lower " <> show e

literal :: Literal -> CodeGen ()
literal n@(Fixnum _) = ins $ "movq $" <> encode n <> ", %rax"
literal (Bool True)  = ins "movq $0x2f, %rax"
literal (Bool False) = ins "movq $0x6f, %rax"
literal c@(Char _)   = ins $ "movq $" <> encode c <> ", %rax"
literal (String s)   = do
  lab <- stringLabel s
  ins $ "leaq " <> encodeUtf8 lab <> "(%rip), %rax"
  ins "orq $3, %rax"

encode :: Literal -> ByteString
encode (Fixnum n)           = fromString $ show $ n * 4
encode (Char c) | isAscii c = fromString $ show (ord c * 256 + 15)
encode l                    = error $ "Unable to encode literal " <> show l

call :: Expr -> [Expr] -> CodeGen ()
call (Sym "print") es = primPrint es
call (Sym "+")     es = primAdd es
call (Sym "-")     es = primSub es
call (Sym "if")    es = formIf es
call e             _  = throwError $ "can't call " <> show e

prologue :: ByteString -> Int -> CodeGen ()
prologue sym space = do
  dir "text"
  dir $ "globl " <> sym
  dir "p2align 4, 0x90"
  label sym
  ins "pushq %rbp"
  ins "movq %rsp, %rbp"
  when (space > 0) $
    ins $ "subq $" <> fromString (show space) <> ", %rsp"

epilogue :: Int -> CodeGen ()
epilogue space = do
  when (space > 0) $
    ins $ "addq $" <> fromString (show space) <> ", %rsp"
  ins "popq %rbp"
  ins "retq"

formIf :: [Expr] -> CodeGen ()
formIf [cond, t, f] = do
  labFalse <- funLabel
  labEnd   <- funLabel
  expr cond
  ins "cmpq $0x6f, %rax"
  ins $ "jz " <> labFalse
  expr t
  ins $ "jmp " <> labEnd
  label labFalse
  expr f
  label labEnd
formIf [cond, t] = do
  labFalse <- funLabel
  expr cond
  ins "cmpq $0x6f, %rax"
  ins $ "jz " <> labFalse
  expr t
  label labFalse
formIf es = throwError $ "if expects two or three arguments, received " <> show (length es)

primPrint :: [Expr] -> CodeGen ()
primPrint [e] = do
  expr e
  ins "movq %rax, %rdi"
  ins "callq _print"
primPrint es = throwError $ "print expects 1 parameter, received " <> show (length es)

primAdd :: [Expr] -> CodeGen ()
primAdd [a, b] = do
  expr a
  withStackSlot $ \slot -> do
    ins $ "movq %rax, " <> slot <> "(%rbp)"
    expr b
    ins $ "addq " <> slot <> "(%rbp), %rax"
primAdd es = throwError $ "+ expects 2 parameters, received " <> show (length es)

primSub :: [Expr] -> CodeGen ()
primSub [a, b] = do
  expr b
  withStackSlot $ \slot -> do
    ins $ "movq %rax, " <> slot <> "(%rbp)"
    expr a
    ins $ "subq " <> slot <> "(%rbp), %rax"
primSub es = throwError $ "- expects 2 parameters, received " <> show (length es)
