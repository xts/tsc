module Core.CodeGen
  ( lowerToAsm
  ) where

import Control.Lens hiding (transform)

import Core.IR
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad
import Core.CodeGen.Primitives
import Core.Decomposer
import Core.Transform (Transform, transform)

-- | Lower a set of functions and data to assembly code.
lowerToAsm :: Monad m => Image -> Transform m ByteString
lowerToAsm (Image fs ss) = transform $ runCodeGen primitives $ do
  dir "text"
  mapM_ (uncurry string) ss
  mapM_ function fs
  entryFunction
  allocFunction
  globalData

-- | Emit a function prologue, its body, and an epilogue.
function :: Function -> CodeGen ()
function f@(Function (Label name) _ es) = do
  contextName .= name
  reservedWords .= reservedStackWords f
  prologue
  mapM_ expr es
  epilogue

-- | Our bridge from C point. Called by the RTS with the address of our heap and
-- stack, which we place into the expected registers.
entryFunction :: CodeGen ()
entryFunction = do
  contextName .= "_scheme_entry"
  reservedWords .= 0
  prologue
  ins "movq %rsi, %rsp" -- Our second argument is the stack ptr.
  ins "movq %rdi, %rsi" -- Our first argument is the heap ptr.
  ins "pushq %rbp"      -- Save our original stack pointer.
  ins "movq %rsp, %rbp"
  ins $ "callq " <> encodeUtf8 (unLabel mainLabel)
  ins "popq %rsp"       -- Restore our original stack pointer.
  ins "xorq %rax, %rax" -- Return 0 to the OS.
  epilogue

-- | Allocate `n` words with 16-byte ealignment. A header will be stored in
-- the preceding word marking this as a memory allocation and including
-- the size. This is to help the GC.
allocFunction :: CodeGen ()
allocFunction = do
  contextName .= "_scheme_alloc"
  prologue

  -- Possibly trigger GC and convert argument from words to bytes.
  ins "pushq %rax"
  maybeTriggerGC
  ins "popq %rdx"
  ins "shlq $3, %rdx"

  -- Align and bump the pointer.
  ins "addq $0x10, %rsi"
  ins "andq $0xfffffffffffffff0, %rsi"
  ins "movq %rsi, %rax"
  ins "addq %rdx, %rsi"

  -- Write the allocation header.
  ins "orq $2, %rdx"
  ins "movq %rdx, -8(%rax)"

  epilogue

globalData :: CodeGen ()
globalData = do
  dir "data"
  global "_scheme_gc_countdown"

-- | Determine hov much pre-allocated stack space the given function needs. Our
-- calling convention dictates that we need space for each of our arguments, plus
-- any let bindings. Since let bindings use explicit stack slots and come after
-- arguments, the answer is the highest let slot, or, if there are no bindings,
-- the number of arguments.
reservedStackWords :: Function -> Int
reservedStackWords (Function _ (Args as) es) =
  execState (transformIr maxBinding es) (length as)
  where
    maxBinding e@(Let vs _) = (forM_ vs $ \(Binding i _) -> modify (`max` i)) $> e
    maxBinding e = pure e
