module Core.CodeGen
  ( lower
  ) where

import Core.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad
import Core.CodeGen.Primitives
import Core.Decomposer
import Core.Extractor (Function(..))

lower :: Image -> Either String ByteString
lower (Image funs strs) = do
  lambdas <- mconcat <$> mapM function funs
  other   <- mconcat <$> mapM (uncurry string) strs
  entry   <- gen' "_scheme_entry" 0 entryFunction
  pure $ other <> lambdas <> entry

function :: Function -> Either String ByteString
function (Function (Label ctx) (Args as) es) = do
  (st, body) <- gen ctx (length as) $ mapM_ expr es
  (_, pre)   <- gen ctx (length as) $ prologue (stackSpace st)
  (_, post)  <- gen ctx (length as) $ epilogue (stackSpace st)
  pure $ pre <> body <> post

gen :: Text -> Int -> CodeGen () -> Either String (Alloc, ByteString)
gen ctx nargs = runCodeGen ctx nargs primitives

gen' :: Text -> Int -> CodeGen () -> Either String ByteString
gen' ctx nargs f = snd <$> gen ctx nargs f

string :: Text -> Label -> Either String ByteString
string s (Label l) = gen' l 0 $ do
  dir $ "globl " <> encodeUtf8 l
  dir "p2align 4, 0x90"
  label $ encodeUtf8 l
  dir $ "asciz \"" <> encodeUtf8 s <> "\""

prologue :: Int -> CodeGen ()
prologue space = do
  name <- encodeUtf8 <$> context
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

entryFunction :: CodeGen ()
entryFunction = do
  prologue 0
  ins "movq %rsi, %rsp" -- Our second argument is the stack ptr.
  ins "movq %rdi, %rsi" -- Our first argument is the heap ptr.
  ins "pushq %rbp"      -- Save our original stack pointer.
  ins "movq %rsp, %rbp"
  ins $ "callq " <> encodeUtf8 (unLabel mainLabel)
  ins "popq %rsp"       -- Restore our original stack pointer.
  ins "xorq %rax, %rax" -- Return 0 to the OS.
  epilogue 0
