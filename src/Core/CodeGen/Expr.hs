module Core.CodeGen.Expr
  ( expr
  , literal
  ) where

import Control.Monad.Except (throwError)
import Data.Text (unpack)

import Core.IR
import Core.CodeGen.Emitters
import Core.CodeGen.Monad

-- | Emit instructions for an expression.
expr :: Expr -> CodeGen ()
expr (Arg i)            = load (Stack i)
expr (CArg i)           = load (Closure i) >> deref
expr (If p t f)         = ifForm p t f
expr (LamDec a f l)     = lambda a f l
expr (Let vs es)        = letForm vs es
expr (List (Prim s:es)) = applyPrimitive s es
expr (List (e:es))      = apply e es
expr (Lit lit)          = literal lit
expr (Prim s)           = throwError $
  "error: indefinite arity primitive cannot be passed as an argument: " <> show s
expr (Var i)            = load (Stack i) >> deref
expr Nil                = nil
expr e                  = throwError $ "Unable to lower " <> show e

-- | Express the given literal.
literal :: Literal -> CodeGen ()
literal (Fixnum k)   = fixnum k
literal (Bool True)  = true
literal (Bool False) = false
literal (Char c)     = char c
literal (String (Right l)) = stringPtr l
literal (String (Left _))  = throwError "String should have been labelized"

-- | Apply the operator to the given set of operands.
apply :: Expr -> [Expr] -> CodeGen ()
apply e es = withComment ("Apply " <> show e) $ withSavedContext $ do
  withComment "Evaluate operator" $ do
    expr e
    ins "pushq %rax"

  allocStack 1 -- Shift stack so that we can push operands directly into place.
  forM_ (zip es [1..]) $ \(e', i) -> do
    withComment ("Evaluate argument " <> show (i :: Int)) $ do
      expr e'
      ins "pushq %rax"
  freeStack $ 1 + length es -- Move stack pointer back.

  ins "popq %rax"
  callClosure

-- | Emit instructions for a primitive function.
applyPrimitive :: Text -> [Expr] -> CodeGen ()
applyPrimitive name es = withComment ("Primitive " <> name) $ lookupPrimitive name >>= \case
  Just (Primitive emitter Indefinite) -> emitter es
  Just (Primitive emitter (Arity n))
    | n == length es -> emitter es
    | otherwise      -> throwError $ "error: " <> unpack name <> " expects " <> show n <> " arguments"
  Nothing -> throwError $ "internal error: no such primitive " <> show name

-- | Allocate and populate a closure.
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

-- | Evaluate a condition and branch.
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

-- | Evaluate expressions and heap-allocate the results to stack positions.
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
