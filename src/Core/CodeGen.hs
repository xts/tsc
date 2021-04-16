module Core.CodeGen
  ( lower
  ) where

import Data.Map qualified as Map

import Core.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad
import Core.CodeGen.Primitives
import Core.Extractor

lower :: [Expr] -> Either String ByteString
lower es = do
  let (es', strLabs) = extractStrings es
  let (es'', lamLabs) = extractLambdas es'
  lams <- lambdas lamLabs
  stringData <- snd <$> gen "strings" 0 (strings strLabs)
  main <- snd <$> function "_scheme_entry" 0 es'' True
  pure $ main <> lams <> stringData

gen :: Text -> Int -> CodeGen () -> Either String (Alloc, ByteString)
gen ctx nargs = runCodeGen ctx nargs primitives

lambdas :: [(Label, Int, [Expr])] -> Either String ByteString
lambdas lams = mconcat . map snd <$> fns
  where fns = forM lams $ \(lb, nargs, es) -> function (unLabel lb) nargs es False

strings :: Map Text Label -> CodeGen ()
strings labels = do
  forM_ (Map.toList labels) $ \(k, Label v) -> do
    dir $ "globl " <> encodeUtf8 v
    dir "p2align 4, 0x90"
    label $ encodeUtf8 v
    dir $ "asciz \"" <> encodeUtf8 k <> "\""

function :: Text -> Int -> [Expr] -> Bool -> Either String (Alloc, ByteString)
function ctx nargs es isMain = do
  (st, body) <- gen ctx nargs $ mapM_ expr es
  (_, pre)   <- gen ctx nargs $ prologue (stackSpace st) isMain
  (_, post)  <- gen ctx nargs $ epilogue (stackSpace st) isMain
  pure (st, pre <> body <> post)

prologue :: Int -> Bool -> CodeGen ()
prologue space isMain = do
  name <- encodeUtf8 <$> context
  dir "text"
  dir $ "globl " <> name
  dir "p2align 4, 0x90"
  label name
  ins "pushq %rbp"
  ins "movq %rsp, %rbp"
  when isMain $ do
    ins "movq %rsi, %rsp" -- Our second argument is the stack ptr.
    ins "movq %rdi, %rsi" -- Our first argument is the heap ptr.
    ins "pushq %rbp"      -- Save our original stack pointer.
    ins "movq %rsp, %rbp"
  when (space > 0) $
    ins $ "subq $" <> fromString (show space) <> ", %rsp"

epilogue :: Int -> Bool -> CodeGen ()
epilogue space isMain = do
  when (space > 0) $
    ins $ "addq $" <> fromString (show space) <> ", %rsp"
  when isMain $ do
    ins "popq %rsp"       -- Restore our original stack pointer.
    ins "xorq %rax, %rax" -- Return 0 to the OS.
  ins "popq %rbp"
  ins "retq"
