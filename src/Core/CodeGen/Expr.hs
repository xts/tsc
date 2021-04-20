module Core.CodeGen.Expr
  ( expr
  , literal
  , varAddr
  ) where

import Control.Monad.Except (throwError)
import Data.Char (isAscii)

import Core.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Monad

expr :: Expr -> CodeGen ()
expr Nil           = ins "movq $0x3f, %rax"
expr (Sym s)       = throwError $ "Run resolver pass to resolve " <> show s
expr (Lit lit)     = literal lit
expr (List (x:xs)) = form x xs
expr (Let vs es)   = letForm vs es
expr (Arg i)       = arg i
expr (Var i)       = varSlot (Stack i)
expr (CArg i)      = varSlot (Closure i)
expr (Prim _)      = throwError "Primitives are not first class objects"
expr (LamDec a f l)= lambda a f l
expr (If p t f)    = ifForm p t f
expr e             = throwError $ "Unable to lower " <> show e

literal :: Literal -> CodeGen ()
literal n@(Fixnum _) = ins $ "movq $" <> encode n <> ", %rax"
literal (Bool True)  = ins "movq $0x2f, %rax"
literal (Bool False) = ins "movq $0x6f, %rax"
literal c@(Char _)   = ins $ "movq $" <> encode c <> ", %rax"
literal (String (Right l)) = do
  ins $ "leaq " <> encodeUtf8 (unLabel l) <> "(%rip), %rax"
  ins "orq $3, %rax"
literal (String (Left _)) = throwError "String should have been labelized"

encode :: Literal -> ByteString
encode (Fixnum n)           = fromString $ show $ n * 4
encode (Char c) | isAscii c = fromString $ show (ord c * 256 + 15)
encode l                    = error $ "Unable to encode literal " <> show l

form :: Expr -> [Expr] -> CodeGen ()
form (Prim s) es = lookupPrimitive s >>= \case
  Just prim -> prim es
  Nothing   -> throwError $ "internal error, no such primitive " <> show s
form e es = do
  pushArgs es
  expr e
  callClosure

callClosure :: CodeGen ()
callClosure = do
  ins "subq $6, %rax"
  ins "movq %rax, %rdi"
  ins "movq (%rax), %rax"
  ins "callq *%rax"

heapAlign16 :: CodeGen ()
heapAlign16 = do
  ins "movq %rsi, %rax"
  ins "andq $0xf, %rax"
  ins "addq $0x10, %rsi"
  ins "subq %rax, %rsi"

heapAllocate :: Int -> CodeGen ()
heapAllocate k = do
  ins "movq %rsi, %rax"
  ins $ "addq $" <> show k <> ", %rsi"

varSlot :: Var -> CodeGen ()
varSlot s = varAddr s >> ins "movq (%rax), %rax"

varAddr :: Var -> CodeGen ()
varAddr (Stack slot) = ins $ "movq " <> stackSlot slot <> "(%rbp), %rax"
varAddr (Closure slot) = do
  ins "movq %rdi, %rax"
  ins $ "addq $" <> closureSlot slot <> ", %rax"
  ins "movq (%rax), %rax"

arg :: Int -> CodeGen ()
arg n = ins $ "movq " <> stackSlot n <> "(%rbp), %rax"

lambda :: Args -> FreeArgs -> Label -> CodeGen ()
lambda _ (FreeArgs fs) l = do
  heapAlign16

  -- Store lambda address at word 0.
  ins $ "leaq " <> encodeUtf8 (unLabel l) <> "(%rip), %rax"
  ins "movq %rax, 0(%rsi)"

  -- Store values of free variables in subsequent words.
  forM_ (zip [1..] fs) $ \(i, Place j) -> do
    varAddr (Stack j)
    ins $ "movq %rax, " <> show (i * 8 :: Int) <> "(%rsi)"

  -- Tag the address as a closure.
  heapAllocate $ (1 + length fs) * 8
  ins "orq $6, %rax"

-- The callee expects its arguments starting in stack slot 3. This is
-- because slot 1 is the return address and slot 2 is the rbp save.
pushArgs :: [Expr] -> CodeGen ()
pushArgs es = forM_ (zip es [3..]) $ \(e, i) -> do
    expr e
    ins $ "movq %rax, " <> stackSlot i <> "(%rsp)"

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
      ins "pushq %rsi"
      ins $ "movq %rsi, " <> stackSlot i <> "(%rbp)"
      ins "addq $8, %rsi"
      expr e
      ins "popq %rdx"
      ins "movq %rax, (%rdx)"
    letBind _ = throwError "Run resolver pass to allocate stack space"
