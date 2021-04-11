module Core.CodeGen.Expr
  ( expr
  , literal
  ) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Char (isAscii, ord)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Core.Analyser.AST
import Core.CodeGen.State
import Core.CodeGen.Emitters

expr :: Expr -> CodeGen ()
expr Nil           = ins "movq $0x3f, %rax"
expr (Sym s)       = var s
expr (Lit lit)     = literal lit
expr (List (x:xs)) = form x xs
expr (Arg i)       = ins $ "movq " <> stackSlot i <> "(%rbp), %rax"
expr (Lam l)       = do
  ins $ "leaq " <> encodeUtf8 (unLabel l) <> "(%rip), %rax"
  ins "orq $6, %rax" -- Tag as closure.
expr e             = throwError $ "Unable to lower " <> show e

var :: Text -> CodeGen ()
var v = lookupVariable v >>= \case
  Just slot -> ins $ "movq " <> stackSlot slot <> "(%rbp), %rax"
  Nothing   -> throwError $ "no such binding " <> show v

literal :: Literal -> CodeGen ()
literal n@(Fixnum _) = ins $ "movq $" <> encode n <> ", %rax"
literal (Bool True)  = ins "movq $0x2f, %rax"
literal (Bool False) = ins "movq $0x6f, %rax"
literal c@(Char _)   = ins $ "movq $" <> encode c <> ", %rax"
literal (String l) = do
  ins $ "leaq " <> encodeUtf8 (unLabel l) <> "(%rip), %rax"
  ins "orq $3, %rax"

encode :: Literal -> ByteString
encode (Fixnum n)           = fromString $ show $ n * 4
encode (Char c) | isAscii c = fromString $ show (ord c * 256 + 15)
encode l                    = error $ "Unable to encode literal " <> show l

form :: Expr -> [Expr] -> CodeGen ()
form (Sym "if")  es = formIf es
form (Sym "let") es = formLet es
form (Lam l)     es = lambda l es
form (Sym s)     es = fvar s es
form e _  = throwError $ "don't know how to evaluate form " <> show e

fvar :: Text -> [Expr] -> CodeGen ()
fvar s es = lookupPrimitive s >>= \case
  Just prim -> prim es
  Nothing   -> do
    pushArgs es
    var s
    ins "subq $6, %rax"
    ins "callq *%rax"

-- The callee expects its arguments starting in stack slot 3. This is
-- because slot 1 is the return address and slot 2 is the rbp save.
pushArgs :: [Expr] -> CodeGen ()
pushArgs es = forM_ (zip es [3..]) $ \(e, i) -> do
    expr e
    ins $ "movq %rax, " <> stackSlot i <> "(%rsp)"

lambda :: Label -> [Expr] -> CodeGen ()
lambda (Label l) es = do
  pushArgs es
  ins $ "callq " <> encodeUtf8 l

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

formIf :: [Expr] -> CodeGen ()
formIf [cond, t, f] = do
  labFalse <- funLabel
  labEnd   <- funLabel
  expr cond
  ins "cmpq $0x6f, %rax"
  ins $ "jz " <> labFalse
  expr t
  ins $ "jmp " <> labEnd
  label labFalse
  expr f
  label labEnd
formIf [cond, t] = do
  labFalse <- funLabel
  expr cond
  ins "cmpq $0x6f, %rax"
  ins $ "jz " <> labFalse
  expr t
  label labFalse
formIf es = throwError $ "if expects two or three arguments, received " <> show (length es)

unpackVars :: Expr -> CodeGen [(Text, Expr)]
unpackVars (List vars) = go vars
  where
    go :: [Expr] -> CodeGen [(Text, Expr)]
    go (List [Sym s, e] : vs) = ((s, e):)   <$> go vs
    go (Sym s           : vs) = ((s, Nil):) <$> go vs
    go []                     = pure []
    go (e : _)                = throwError $ "invalid let var form " <> show e
unpackVars Nil                = pure []
unpackVars _                  = throwError "invalid let vars form"

formLet :: [Expr] -> CodeGen ()
formLet (vs:b:bs) = do
  vars <- unpackVars vs
  forM_ vars $ \(name, e) -> do
    slot <- allocStackSlot
    addVariable name slot
    expr e
    ins $ "movq %rax, " <> stackSlot slot <> "(%rbp)"
  mapM_ expr (b:bs)
  forM_ vars $ \(name, _) -> do
    delVariable name
    freeStackSlot
formLet _ = throwError $ "let expects at least two forms"
