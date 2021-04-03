module Core.CodeGen
  ( lower
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Core.AST

lower :: Expr -> Either String ByteString
lower expression = Right $
  prologue "_entry_function"
  <> expr expression
  <> epilogue

expr :: Expr -> ByteString
expr (Lit (Fixnum n))   = ins $ "movl $" <> fromString (show $ n * 4) <> ", %eax"
expr (Lit (Bool True))  = ins "movl $0x2f, %eax"
expr (Lit (Bool False)) = ins "movl $0x6f, %eax"
expr Nil                = ins "movl $0x3f, %eax"
expr _                  = error "Implement me"

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
