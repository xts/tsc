module Core.CodeGen
  ( lower
  ) where

import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Core.Analyser
import Core.Analyser.AST
import Core.Parser.AST qualified as P
import Core.CodeGen.State
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Primitives

lower :: [P.Expr] -> Either String ByteString
lower es = do
  let (es', Labels stringLabels) = analyse es
  code       <- snd <$> function "_scheme_entry" es' True
  stringData <- snd <$> gen "strings" (strings stringLabels)
  pure $ code <> stringData

gen :: Text -> CodeGen () -> Either String (State, ByteString)
gen ctx = runCodeGen ctx primitives

strings :: Map Text Label -> CodeGen ()
strings labels = do
  forM_ (Map.toList labels) $ \(k, Label v) -> do
    sep
    dir $ "globl " <> encodeUtf8 v
    dir "p2align 4, 0x90"
    label $ encodeUtf8 v
    dir $ "asciz \"" <> encodeUtf8 k <> "\""

function :: Text -> [Expr] -> Bool -> Either String (State, ByteString)
function ctx es isMain = do
  (st, body) <- gen ctx $ mapM_ expr es
  (_, pre)   <- gen ctx $ prologue (stackSpace st) isMain
  (_, post)  <- gen ctx $ epilogue (stackSpace st) isMain
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
  when (space > 0) $
    ins $ "subq $" <> fromString (show space) <> ", %rsp"
  when isMain $ ins "movq %rdi, %rsi" -- Our argument is the heap ptr.

epilogue :: Int -> Bool -> CodeGen ()
epilogue space isMain = do
  when isMain $ ins "xorq %rax, %rax" -- Return 0 to the OS.
  when (space > 0) $
    ins $ "addq $" <> fromString (show space) <> ", %rsp"
  ins "popq %rbp"
  ins "retq"
