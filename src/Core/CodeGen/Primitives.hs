module Core.CodeGen.Primitives
  ( primitives
  , isPrimitive
  ) where

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Prelude hiding (print)

import Core.IR
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
add es = do
  ins "pushq $0"
  forM_ es $ \e -> do
    expr e
    ins "addq %rax, (%rsp)"
  ins "popq %rax"

sub :: [Expr] -> CodeGen ()
sub (e : es) = do
  expr e
  ins "push %rax"
  forM_ es $ \e' -> do
    expr e'
    ins "subq %rax, (%rsp)"
  ins "popq %rax"
sub [] = throwError "- expects at least one argument"

lessThan :: [Expr] -> CodeGen ()
lessThan [a, b] = do
  lab <- funLabel
  expr b
  ins "push %rax"
  expr a
  ins "subq (%rsp), %rax"
  false
  ins $ "jge " <> lab
  true
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
addr (Var i)  = load (Stack i)
addr (CArg i) = load (Closure i)
addr e        = error $ "Don't know how to find the address of " <> show e
