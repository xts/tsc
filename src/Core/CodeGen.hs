module Core.CodeGen
  ( lower
  ) where

import Control.Monad (forM_, when)
import Control.Monad.RWS (RWST, execRWST, ask, tell, get, modify)
import Control.Monad.Except (Except, runExcept, throwError)

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Char (isAscii, ord)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Core.AST

data Env = Env
  { _envFunctions :: Map Text Text -- ^ Function name -> function label.
  , envContext    :: Text          -- ^ Current function.
  }

data Allocations = Allocations
  { stCurStack     :: Int           -- ^ Next free stack slot.
  , stMaxStack     :: Int           -- ^ Peak stack usage.
  , stStringLabels :: Map Text Text -- ^ Constant string -> string label.
  , stNextLabel    :: Int           -- ^ Suffix of next local code label.
  , stVariables    :: [(Text, Int)] -- ^ Variable name -> stack index.
  }

type CodeGen a = RWST Env ByteString Allocations (Except String) a

runCodeGen :: Text -> CodeGen () -> Either String (Allocations, ByteString)
runCodeGen ctx f = runExcept $ execRWST f env state
  where
    env   = Env Map.empty ctx
    state = Allocations 0 0 Map.empty 0 []

emit :: ByteString -> CodeGen ()
emit = tell

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
  sp <- stCurStack <$> get
  setStack (succ sp)
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
stackSlot = fromString . show . ((-8) *) . succ

stackSpace :: Allocations -> Int
stackSpace = to16s . (8*) . stMaxStack
  where
    to16s n = let r = n `rem` 16 in if r == 0 then n else n + 16 - r

stringLabel :: Text -> CodeGen Text
stringLabel text = do
  labels <- stStringLabels <$> get
  case Map.lookup text labels of
    Just lab -> pure lab
    Nothing    -> do
      let lab = "_string_" <> pack (show $ Map.size labels)
      modify $ \st -> st { stStringLabels = Map.insert text lab labels }
      pure lab

funLabel :: CodeGen ByteString
funLabel = do
  context <- envContext <$> ask
  n <- stNextLabel <$> get
  modify $ \st -> st { stNextLabel = succ n }
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
  (_, stringData) <- runCodeGen mempty $ strings $ stStringLabels alloc
  pure $ code <> stringData

function :: Text -> Expr -> Either String (Allocations, ByteString)
function name e = do
  (alloc, body) <- runCodeGen name $ expr e
  (_, func) <- runCodeGen name $ do
    prologue (stackSpace alloc)
    emit body
    ins "xorq %rax, %rax"
    epilogue (stackSpace alloc)
  pure (alloc, func)

expr :: Expr -> CodeGen ()
expr Nil           = ins "movq $0x3f, %rax"
expr (Lit lit)     = literal lit
expr (List (x:xs)) = form x xs
expr (Sym s)       = lookupVariable s >>= \case
  Just slot -> ins $ "movq " <> stackSlot slot <> "(%rbp), %rax"
  Nothing   -> throwError $ "no such binding " <> show s
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

form :: Expr -> [Expr] -> CodeGen ()
form (Sym "if")    es = formIf es
form (Sym "let")   es = formLet es
form (Sym "print") es = primPrint es
form (Sym "+")     es = primAdd es
form (Sym "-")     es = primSub es
form (Sym "<")     es = primLessThan es
form e             _  = throwError $ "don't know how to evaluate form " <> show e

prologue :: Int -> CodeGen ()
prologue space = do
  name <- encodeUtf8 . envContext <$> ask
  dir "text"
  dir $ "globl " <> name
  dir "p2align 4, 0x90"
  label name
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

unpackVars :: Expr -> CodeGen [(Text, Expr)]
unpackVars (List vars) = go vars
  where
    go :: [Expr] -> CodeGen [(Text, Expr)]
    go (List [Sym s, e] : vs) = ((s, e):)   <$> go vs
    go (Sym s           : vs) = ((s, Nil):) <$> go vs
    go []                     = pure []
    go (e : _)                = throwError $ "invalid let var form " <> show e
unpackVars Nil                = pure []
unpackVars _                  = throwError "invalid let vars form"

formLet :: [Expr] -> CodeGen ()
formLet [vs, body] = do
  vars <- unpackVars vs
  forM_ vars $ \(name, e) -> do
    slot <- allocStackSlot
    addVariable name slot
    expr e
    ins $ "movq %rax, " <> stackSlot slot <> "(%rbp)"
  expr body
  forM_ vars $ \(name, _) -> do
    delVariable name
    freeStackSlot
formLet es = throwError $ "let expects two forms, received " <> show (length es)

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

primLessThan :: [Expr] -> CodeGen ()
primLessThan [a, b] = do
  lab <- funLabel
  primSub [a, b]
  literal $ Bool False
  ins $ "jg " <> lab
  literal $ Bool True
  label lab
primLessThan es = throwError $ "< expects 2 arguments, received " <> show (length es)
