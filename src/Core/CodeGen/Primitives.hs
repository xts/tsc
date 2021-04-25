module Core.CodeGen.Primitives
  ( primitives
  , isPrimitive
  ) where

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Prelude hiding (error, print)

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
  , ("read-char", readChar)
  , ("error", error)
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
