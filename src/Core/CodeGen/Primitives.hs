module Core.CodeGen.Primitives
  ( primitives
  ) where

import Control.Monad.Except (throwError)
import Data.Map (Map, fromList)
import Data.Text (Text)
import Prelude hiding (lookup, print)

import Core.Parser.AST
import Core.CodeGen.State
import Core.CodeGen.Emitters
import Core.CodeGen.Expr

primitives :: Map Text Primitive
primitives = fromList
  [ ("print", print)
  , ("+", add)
  , ("-", sub)
  , ("<", lessThan)
  ]

print :: [Expr] -> CodeGen ()
print [e] = do
  expr e
  ins "movq %rax, %rdi" -- argument to print
  ins "pushq %rsi"      -- save heap ptr
  ins "subq $8, %rsp"   --   align stack to 16
  ins "callq _print"
  ins "addq $8, %rsp"   -- restore heap ptr
  ins "popq %rsi"
print es = throwError $ "print expects 1 parameter, received " <> show (length es)

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
