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
  prologue "_main"
  <> expr 1 expression
  <> ins "xorl %eax, %eax"
  <> epilogue

expr :: Int -> Expr -> Code
expr _  Nil           = ins "movl $0x3f, %eax"
expr _  (Lit lit)     = literal lit
expr sp (List (x:xs)) = call sp x xs
expr _  e             = Error $ "Unable to lower " <> show e

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

call :: Int -> Expr -> [Expr] -> Code
call sp (Sym "print") es = primPrint sp es
call sp (Sym "+")     es = primAdd sp es
call sp (Sym "-")     es = primSub sp es
call _  e             _  = Error $ "can't call " <> show e

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

primPrint :: Int -> [Expr] -> Code
primPrint sp [e] =
  expr sp e
  <> ins "movl %eax, %edi"
  <> ins "callq _print"
primPrint _ es = Error $ "print expects 1 parameter, received " <> show (length es)

primAdd :: Int -> [Expr] -> Code
primAdd sp [a, b] =
  expr sp a
  <> ins ("movl %eax, " <> (fromString . show $ -4 * sp) <> "(%rbp)")
  <> expr (sp + 1) b
  <> ins ("addl " <> (fromString . show $ -4 * sp) <> "(%rbp), %eax")
primAdd _ es = Error $ "+ expects 2 parameters, received " <> show (length es)

primSub :: Int -> [Expr] -> Code
primSub sp [a, b] =
  expr sp b
  <> ins ("movl %eax, " <> (fromString . show $ -4 * sp) <> "(%rbp)")
  <> expr (sp + 1) a
  <> ins ("subl " <> (fromString . show $ -4 * sp) <> "(%rbp), %eax")
primSub _ es = Error $ "- expects 2 parameters, received " <> show (length es)
