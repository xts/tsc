module Core.CodeGen
  ( lowerToAsm
  ) where

import Core.IR
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad
import Core.CodeGen.Primitives
import Core.Decomposer
import Core.Transform (Transform, transform)

lowerToAsm :: Monad m => Image -> Transform m ByteString
lowerToAsm (Image fs ss) = transform $ runCodeGen primitives $ do
  dir "text"
  mapM_ (uncurry string) ss
  mapM_ function fs
  entryFunction

function :: Function -> CodeGen ()
function f@(Function (Label ctx) _ es) = do
  let rsw = reservedStackWords f
  setContext ctx
  prologue rsw
  mapM_ expr es
  epilogue rsw

entryFunction :: CodeGen ()
entryFunction = do
  setContext "_scheme_entry"
  prologue 0
  ins "movq %rsi, %rsp" -- Our second argument is the stack ptr.
  ins "movq %rdi, %rsi" -- Our first argument is the heap ptr.
  ins "pushq %rbp"      -- Save our original stack pointer.
  ins "movq %rsp, %rbp"
  ins $ "callq " <> encodeUtf8 (unLabel mainLabel)
  ins "popq %rsp"       -- Restore our original stack pointer.
  ins "xorq %rax, %rax" -- Return 0 to the OS.
  epilogue 0

reservedStackWords :: Function -> Int
reservedStackWords (Function _ (Args as) es) =
  length as + execState (traverseIr go es) 0
  where
    go :: Expr -> State Int Expr
    go e@(Let vs _) = (forM_ vs $ \(Binding i _) -> modify (`max` i)) $> e
    go e = pure e
