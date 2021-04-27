module Core.CodeGen.Expr
  ( expr
  , literal
  ) where

import Control.Monad.Except (throwError)

import Core.IR
import Core.CodeGen.Emitters
import Core.CodeGen.Monad

expr :: Expr -> CodeGen ()
expr Nil           = nil
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
lambda _ (FreeArgs fs) lab = withComment ("Allocate closure for " <> unLabel lab) $ do
  alloc Align16 $ 1 + length fs
  ins "movq %rax, %rbx"

  withComment "Store function pointer" $ do
    labelAddr lab
    storeVia "%rbx" (Closure 0)

  forM_ (zip [1..] fs) $ \(i, e) -> do
    withComment ("Store argument " <> show i) $ do
      case e of
        Var j  -> load (Stack j)
        Arg j  -> load (Stack j) >> box
        CArg j -> load (Closure j)
        e' -> throwError $ "internal error: cannot allocate to closure: " <> show e'
      storeVia "%rbx" (Closure i)

  ins "movq %rbx, %rax"
  tagClosure

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
    letBind (Binding i e) = withComment ("Bind %V" <> show i <> " to " <> show e) $ do
      alloc Align8 1
      ins "pushq %rax"
      store (Stack i)
      expr e
      ins "popq %rdx"
      ins "movq %rax, (%rdx)"
