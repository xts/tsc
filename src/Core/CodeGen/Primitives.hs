module Core.CodeGen.Primitives
  ( primitives
  , isPrimitive
  ) where

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Prelude hiding (print)

import Core.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad

isPrimitive :: Text -> Bool
isPrimitive s = Map.member s primitives

primitives :: Map Text Primitive
primitives = fromList
  [ ("display", display)
  , ("+", add)
  , ("-", sub)
  , ("<", lessThan)
  , ("set!", set)
  ]

display :: [Expr] -> CodeGen ()
display [e] = do
  expr e
  ins "pushq %rsi"      -- save heap ptr.
  ins "pushq %rdi"      -- save closure ptr.
  ins "movq %rax, %rdi" -- argument to print.

  ins "pushq %rbp"      -- align stack to 16 bytes.
  ins "movq %rsp, %rbp"
  ins "subq $8, %rsp"
  ins "andq $0xfffffffffffffff0, %rsp"

  ins "callq _print"

  ins "movq %rbp, %rsp" -- restore stack.
  ins "popq %rbp"

  ins "popq %rdi"       -- restore closure ptr.
  ins "popq %rsi"       -- restore heap ptr.
display es = throwError $ "display expects 1 parameter, received " <> show (length es)

add :: [Expr] -> CodeGen ()
add [a, b] = do
  expr a
  ins "push %rax"
  expr b
  ins "addq (%rsp), %rax"
  ins "addq $8, %rsp"
add es = throwError $ "+ expects 2 parameters, received " <> show (length es)

sub :: [Expr] -> CodeGen ()
sub [a, b] = do
  expr b
  ins "push %rax"
  expr a
  ins "subq (%rsp), %rax"
  ins "addq $8, %rsp"
sub es = throwError $ "- expects 2 parameters, received " <> show (length es)

lessThan :: [Expr] -> CodeGen ()
lessThan [a, b] = do
  lab <- funLabel
  expr b
  ins "push %rax"
  expr a
  ins "subq (%rsp), %rax"
  literal $ Bool False
  ins $ "jge " <> lab
  literal $ Bool True
  label lab
  ins "addq $8, %rsp"
lessThan es = throwError $ "< expects 2 arguments, received " <> show (length es)

set :: [Expr] -> CodeGen ()
set [v, e] = do
  expr e
  ins "pushq %rax"
  addr v
  ins "popq %rdx"
  ins "movq %rdx, (%rax)"
set es = throwError $ "set! expects 2 arguments, received " <> show (length es)

addr :: Expr -> CodeGen ()
addr (Sym _) = throwError "Use resolver"
addr (CArg n) = varAddr (Closure n)
addr (Var n)  = varAddr (Stack n)
addr e = throwError $ "Don't know how to resolve the address of " <> show e
