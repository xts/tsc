-- Need ability to convert strings to constants
{--
literal :: Literal -> Code
literal (String s) = do
    label <- registerString s
    ins $ "movq " <> label <> "(%rip), %eax"
--}

module Core.CodeGen
  ( lower
  ) where

import Control.Monad.RWS (RWST, execRWST, tell, get, modify)
import Control.Monad.Except (Except, runExcept, throwError)

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Char (isAscii, ord)
import Data.Either.Combinators (mapRight)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

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
    state = Allocations 1 2 Map.empty 0

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
stackSlot = fromString . show . ((-4) *) . succ

ins :: ByteString -> CodeGen ()
ins text = emit $ indent <> text <> "\n"

dir :: ByteString -> CodeGen ()
dir name = emit $ indent <> "." <> name <> "\n"

label :: ByteString -> CodeGen ()
label name = emit $ name <> ":\n"

indent :: ByteString
indent = "    "

lower :: Expr -> Either String ByteString
lower expression = do
  (alloc, code) <- runCodeGen "_main" $ expr expression
  mapRight snd $ runCodeGen "_main" $ do
    prologue "_main" (stMaxStack alloc)
    emit code
    ins "xorl %eax, %eax"
    epilogue (stMaxStack alloc)

expr :: Expr -> CodeGen ()
expr Nil           = ins "movl $0x3f, %eax"
expr (Lit lit)     = literal lit
expr (List (x:xs)) = call x xs
expr e             = throwError $ "Unable to lower " <> show e

literal :: Literal -> CodeGen ()
literal n@(Fixnum _) = ins $ "movl $" <> encode n <> ", %eax"
literal (Bool True)  = ins "movl $0x2f, %eax"
literal (Bool False) = ins "movl $0x6f, %eax"
literal c@(Char _)   = ins $ "movl $" <> encode c <> ", %eax"
literal e            = throwError $ "Unable to encode " <> show e

encode :: Literal -> ByteString
encode (Fixnum n)           = fromString $ show $ n * 4
encode (Char c) | isAscii c = fromString $ show (ord c * 256 + 15)
encode l                    = error $ "Unable to encode literal " <> show l

call :: Expr -> [Expr] -> CodeGen ()
call (Sym "print") es = primPrint es
call (Sym "+")     es = primAdd es
call (Sym "-")     es = primSub es
call e             _  = throwError $ "can't call " <> show e

prologue :: ByteString -> Int -> CodeGen ()
prologue sym stackSpace = do
  dir "text"
  dir $ "globl " <> sym
  dir "p2align 4, 0x90"
  label sym
  ins "pushq %rbp"
  ins "movq %rsp, %rbp"
  ins $ "subq $" <> fromString (show $ stackSpace * 8) <> ", %rsp"

epilogue :: Int -> CodeGen ()
epilogue stackSpace = do
  ins $ "addq $" <> fromString (show $ stackSpace * 8) <> ", %rsp"
  ins "popq %rbp"
  ins "retq"

primPrint :: [Expr] -> CodeGen ()
primPrint [e] = do
  expr e
  ins "movl %eax, %edi"
  ins "callq _print"
primPrint es = throwError $ "print expects 1 parameter, received " <> show (length es)

primAdd :: [Expr] -> CodeGen ()
primAdd [a, b] = do
  expr a
  withStackSlot $ \slot -> do
    ins $ "movl %eax, " <> slot <> "(%rbp)"
    expr b
    ins $ "addl " <> slot <> "(%rbp), %eax"
primAdd es = throwError $ "+ expects 2 parameters, received " <> show (length es)

primSub :: [Expr] -> CodeGen ()
primSub [a, b] = do
  expr b
  withStackSlot $ \slot -> do
    ins $ "movl %eax, " <> slot <> "(%rbp)"
    expr a
    ins $ "subl " <> slot <> "(%rbp), %eax"
primSub es = throwError $ "- expects 2 parameters, received " <> show (length es)
