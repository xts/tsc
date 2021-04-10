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

import Core.AST
import Core.CodeGen.State
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Primitives

lower :: [Expr] -> Either String ByteString
lower es = do
  (alloc, code)   <- do
    function "_scheme_entry" es True
  (_, stringData) <- runCodeGen mempty primitives $ strings $ stStringLabels alloc
  pure $ code <> stringData

strings :: Map Text Text -> CodeGen ()
strings labels = do
  forM_ (Map.toList labels) $ \(k, v) -> do
    sep
    dir $ "globl " <> encodeUtf8 v
    dir "p2align 4, 0x90"
    label $ encodeUtf8 v
    dir $ "asciz \"" <> encodeUtf8 k <> "\""

function :: Text -> [Expr] -> Bool -> Either String (State, ByteString)
function name es isMain = do
  (alloc, body) <- runCodeGen name primitives $ mapM_ expr es
  (_, func) <- runCodeGen name primitives $ do
    prologue (stackSpace alloc)
    when isMain $ ins "movq %rdi, %rsi" -- Our argument is the heap ptr.
    emit body
    when isMain $ ins "xorq %rax, %rax" -- Return 0 to the OS.
    epilogue (stackSpace alloc)
  pure (alloc, func)

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
