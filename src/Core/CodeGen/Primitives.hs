module Core.CodeGen.Primitives
  ( primitives
  ) where

import Control.Monad.Except (throwError)
import Prelude hiding (print)

import Core.Analyser.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad

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
  withStackSlot $ \slot -> do
    ins $ "movq %rax, " <> slot <> "(%rbp)"
    expr b
    ins $ "addq " <> slot <> "(%rbp), %rax"
add es = throwError $ "+ expects 2 parameters, received " <> show (length es)

sub :: [Expr] -> CodeGen ()
sub [a, b] = do
  expr b
  withStackSlot $ \slot -> do
    ins $ "movq %rax, " <> slot <> "(%rbp)"
    expr a
    ins $ "subq " <> slot <> "(%rbp), %rax"
sub es = throwError $ "- expects 2 parameters, received " <> show (length es)

lessThan :: [Expr] -> CodeGen ()
lessThan [a, b] = do
  lab <- funLabel
  sub [a, b]
  literal $ Bool False
  ins $ "jg " <> lab
  literal $ Bool True
  label lab
lessThan es = throwError $ "< expects 2 arguments, received " <> show (length es)

set :: [Expr] -> CodeGen ()
set [Sym v, e] = lookupVariable v >>= \case
  Just slot -> do
    expr e
    ins "pushq %rax"
    varAddr slot
    ins "popq %rdx"
    ins "movq %rdx, (%rax)"
  Nothing -> throwError $ "No such variable: " <> show v
set es = throwError $ "set! expects 2 arguments, received " <> show (length es)
