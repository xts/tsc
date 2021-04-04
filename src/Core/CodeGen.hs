module Core.CodeGen
  ( lower
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import Core.AST

data Code
  = Code ByteString
  | Error String

instance Semigroup Code where
  (<>) = mappend

instance Monoid Code where
  mempty = Code mempty
  Error e  `mappend` _       = Error e
  Code _   `mappend` Error e = Error e
  Code a   `mappend` Code b  = Code (a <> b)

toEither :: Code -> Either String ByteString
toEither (Code c)  = Right c
toEither (Error e) = Left e

lower :: Expr -> Either String ByteString
lower expression = toEither $
  prologue "_entry_function"
  <> expr expression
  <> epilogue

expr :: Expr -> Code
expr Nil           = ins "movl $0x3f, %eax"
expr (Lit lit)     = literal lit
expr (List (x:xs)) = call x xs
expr e             = Error $ "Unable to lower " <> show e

literal :: Literal -> Code
literal (Fixnum n)   = ins $ "movl $" <> fromString (show $ n * 4) <> ", %eax"
literal (Bool True)  = ins "movl $0x2f, %eax"
literal (Bool False) = ins "movl $0x6f, %eax"
literal e            = Error $ "Unable to encode " <> show e

call :: Expr -> [Expr] -> Code
call (Sym "print") [e] =
  expr e
  <> ins "movl %eax, %edi"
  <> ins "callq _print"
call (Sym "print") es  = Error $ "print expects one parameter, not " <> show (length es)
call e        _        = Error $ "can't call " <> show e

prologue :: ByteString -> Code
prologue sym =
  dir "text"
  <> dir ("globl " <> sym)
  <> dir "p2align 4, 0x90"
  <> label sym
  <> ins "pushq %rbp"
  <> ins "movq %rsp, %rbp"

epilogue :: Code
epilogue =
  ins "popq %rbp"
  <> ins "retq"

ins :: ByteString -> Code
ins text = Code $ indent <> text <> "\n"

dir :: ByteString -> Code
dir name = Code $ indent <> "." <> name <> "\n"

label :: ByteString -> Code
label sym = Code $ sym <> ":\n"

indent :: ByteString
indent = "    "
