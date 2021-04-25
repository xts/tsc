module Core.CodeGen.Primitives
  ( primitives
  , isPrimitive
  ) where

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Prelude hiding (and, error, print)

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
  , ("*", mul)
  , ("and", and)
  , ("eq", eq)
  , ("=", eq)
  , ("<", lessThan)
  , ("set!", set)
  , ("read-char", readChar)
  , ("error", error)
  , ("number->char", numberToChar)
  , ("char->number", charToNumber)
  ]

display :: [Expr] -> CodeGen ()
display [e] = do
  expr e
  callC "_print"
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

mul :: [Expr] -> CodeGen ()
mul es = do
  ins "pushq $1"
  forM_ es $ \e -> do
    expr e
    untagFixnum
    ins "mulq (%rsp)"
    ins "movq %rax, (%rsp)"
  ins "popq %rax"
  tagFixnum

and :: [Expr] -> CodeGen ()
and es = do
  endLabel   <- funLabel
  forM_ es $ \e -> do
    expr e
    ins "cmpq $0x6f, %rax"
    ins $ "je " <> endLabel
  true
  label endLabel

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
addr e        = throwError $ "Don't know how to find the address of " <> show e

readChar :: [Expr] -> CodeGen ()
readChar [] = do
  callC "_read_char"
  tagChar
readChar _ = throwError "read-char takes no arguments"

error :: [Expr] -> CodeGen ()
error [e] = do
  display [e]
  ins "movq $1, %rdi"
  callC "_exit"
error _ = throwError "error: error currently only takes one argument"

numberToChar :: [Expr] -> CodeGen ()
numberToChar [e] = do
  expr e
  untagFixnum
  ins "andq $0xff, %rax"
  tagChar
numberToChar _ = throwError "error: number->char takes one argument"

charToNumber :: [Expr] -> CodeGen ()
charToNumber [e] = do
  expr e
  untagChar
  tagFixnum
charToNumber _ = throwError "error: char->number takes one argument"

eq :: [Expr] -> CodeGen ()
eq [a, b] = do
  labEnd <- funLabel
  expr a
  ins "pushq %rax"
  expr b
  ins "popq %rdx"
  ins "movq %rax, %rbx"
  true
  ins "cmpq %rbx, %rdx"
  ins $ "je " <> labEnd
  false
  label labEnd
eq _ = throwError $ "error: eq takes two arguments"
