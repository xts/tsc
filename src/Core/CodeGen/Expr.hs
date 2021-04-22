module Core.CodeGen.Expr
  ( expr
  , literal
  ) where

import Control.Monad.Except (throwError)

import Core.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Monad

expr :: Expr -> CodeGen ()
expr Nil           = nil
expr (Sym s)       = throwError $ "Run resolver pass to resolve " <> show s
expr (Lit lit)     = literal lit
expr (List (x:xs)) = apply x xs
expr (Let vs es)   = letForm vs es
expr (Arg i)       = load (Stack i)
expr (Var i)       = load (Stack i) >> deref
expr (CArg i)      = load (Closure i) >> deref
expr (Prim _)      = throwError "Primitives are not first class objects"
expr (LamDec a f l)= lambda a f l
expr (If p t f)    = ifForm p t f
expr e             = throwError $ "Unable to lower " <> show e

literal :: Literal -> CodeGen ()
literal (Fixnum k)   = fixnum k
literal (Bool True)  = true
literal (Bool False) = false
literal (Char c)     = char c
literal (String (Right l)) = stringPtr l
literal (String (Left _))  = throwError "String should have been labelized"

apply :: Expr -> [Expr] -> CodeGen ()
apply (Prim s) es = primitive s >>= ($ es)
apply e        es = withSavedContext $ do
  forM_ (zip es [1..]) $ \(e', i) -> do
    expr e'
    store (Param i)
  expr e
  callClosure

lambda :: Args -> FreeArgs -> Label -> CodeGen ()
lambda _ (FreeArgs fs) l = withSavedContext $ do
  -- Allocate closure with function pointer and free args.
  alloc Align16 $ 1 + length fs
  ins "movq %rax, %rdi"

  -- Place function pointer at word 1.
  ins $ "leaq " <> encodeUtf8 (unLabel l) <> "(%rip), %rax"
  store (Closure 0)

  -- Store values of free variables in subsequent words.
  forM_ (zip [1..] fs) $ \(i, Place j) -> do
    load (Stack j)
    store (Closure i)

  -- Tag the address as a closure.
  ins "movq %rdi, %rax"
  ins "orq $6, %rax"

_cons :: Text -> Text -> CodeGen ()
_cons a d = do
  ins $ "movq " <> encodeUtf8 a <> ", 0(%rsi)" -- set car
  ins $ "movq " <> encodeUtf8 d <> ", 0(%rsi)" -- set cdr
  ins "movq %rsi, %rax"                        -- set eax to rsi | 1
  ins "orq %eax, 1"
  ins "addq $16, %rsi"                         -- bump heap ptr

-- | Read the first element of a pair. This would normally be at offset 0, but
-- since the pair object is tagged with 0xb001, our offset is at -1.
_car :: CodeGen ()
_car = ins "leaq -1(%rax), %rax"

-- | Read the second element of a pair. This would normally be at offset 8, but
-- since the pair object is tagged with 0xb001, our offset is at 7.
_cdr :: CodeGen ()
_cdr = ins "leaq 7(%rax), %rax)"

ifForm :: Expr -> Expr -> Expr -> CodeGen ()
ifForm p t f = do
  labFalse <- funLabel
  labEnd   <- funLabel
  expr p
  ins "cmpq $0x6f, %rax"
  ins $ "jz " <> labFalse
  expr t
  ins $ "jmp " <> labEnd
  label labFalse
  expr f
  label labEnd

letForm :: [Binding] -> [Expr] -> CodeGen ()
letForm vs es = do
  mapM_ letBind vs
  mapM_ expr es
  where
    letBind (Binding (Place i) e) = do
      alloc Align8 1
      ins "pushq %rax"
      store (Stack i)
      expr e
      ins "popq %rdx"
      ins "movq %rax, (%rdx)"
    letBind _ = throwError "Run resolver pass to allocate stack space"
