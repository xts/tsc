module Compiler
  (compile
  ) where

import AST
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS

compile :: AST -> ByteString
compile _ =
  prologue "_entry_function"
  <> epilogue

prologue :: ByteString -> ByteString
prologue sym =
  dir "text"
  <> dir ("globl " <> sym)
  <> dir "p2align 4, 0x90"
  <> label sym
  <> dir "cfi_startproc"
  <> ins "pushq %rbp"
  <> ins "movq %rsp, %rbp"

epilogue :: ByteString
epilogue =
  ins "popq %rbp"
  <> ins "retq"
  <> dir "cfi_endproc"

ins :: ByteString -> ByteString
ins text = indent <> text <> "\n"

dir :: ByteString -> ByteString
dir name = indent <> "." <> name <> "\n"

label :: ByteString -> ByteString
label sym = sym <> ":\n"

indent :: ByteString
indent = "    "
