module Core.CodeGen
  ( lower
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Char (isAscii, ord)

import Core.AST
import Core.Code

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
literal n@(Fixnum _) = ins $ "movl $" <> encode n <> ", %eax"
literal (Bool True)  = ins "movl $0x2f, %eax"
literal (Bool False) = ins "movl $0x6f, %eax"
literal c@(Char _)   = ins $ "movl $" <> encode c <> ", %eax"
literal e            = Error $ "Unable to encode " <> show e

encode :: Literal -> ByteString
encode (Fixnum n)           = fromString $ show $ n * 4
encode (Char c) | isAscii c = fromString $ show (ord c * 256 + 15)
encode l                    = error $ "Unable to encode literal " <> show l

call :: Expr -> [Expr] -> Code
call (Sym "print") es = primPrint es
call (Sym "+")     es = primAdd es
call (Sym "-")     es = primSub es
call e             _  = Error $ "can't call " <> show e

prologue :: ByteString -> Code
prologue sym =
  dir "text"
  <> dir ("globl " <> sym)
  <> dir "p2align 4, 0x90"
  <> label sym
  <> ins "pushq %rbp"
  <> ins "movq %rsp, %rbp"
  <> ins "subq $16, %rsp"

epilogue :: Code
epilogue =
  ins "addq $16, %rsp"
  <> ins "popq %rbp"
  <> ins "retq"

primPrint :: [Expr] -> Code
primPrint [e] =
  expr e
  <> ins "movl %eax, %edi"
  <> ins "callq _print"
primPrint es = Error $ "print expects 1 parameter, received " <> show (length es)

primAdd :: [Expr] -> Code
primAdd [a, b] =
  expr a
  <> ins "movl %eax, -4(%rbp)"
  <> expr b
  <> ins "addl -4(%rbp), %eax"
primAdd es = Error $ "+ expects 2 parameters, received " <> show (length es)

primSub :: [Expr] -> Code
primSub [a, b] =
  expr a
  <> ins "movl %eax, -4(%rbp)"
  <> expr b
  <> ins "subl -4(%rbp), %eax"
primSub es = Error $ "- expects 2 parameters, received " <> show (length es)
