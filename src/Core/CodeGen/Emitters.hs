module Core.CodeGen.Emitters
  ( Location(..)
  , Alignment(..)
  , nil
  , true
  , false
  , fixnum
  , char
  , stringPtr
  , load
  , store
  , storeVia
  , deref
  , callClosure
  , withSavedContext
  , moveInt
  , define
  , prologue
  , epilogue
  , tagClosure
  , untagClosure
  , tagChar
  , untagChar
  , tagFixnum
  , untagFixnum
  , tagPair
  , untagPair
  , labelAddr
  , string
  , allocStack
  , freeStack
  , alloc
  , box
  , callC
  , ins
  , sep
  , dir
  , label
  , indent
  , withComment
  , comment
  ) where

import Control.Monad.Except (throwError)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAscii)

import Core.AST
import Core.CodeGen.Monad

data Location
  = Closure Int
  | Param Int
  | Stack Int

data Alignment = Align8 | Align16

nil :: CodeGen ()
nil = moveInt 0x3f "%rax"

true :: CodeGen ()
true = moveInt 0x2f "%rax"

false :: CodeGen ()
false = moveInt 0x6f "%rax"

fixnum :: Int -> CodeGen ()
fixnum k = moveInt (4 * k) "%rax"

char :: Char -> CodeGen ()
char c | isAscii c = moveInt (ord c * 256 + 15) "%rax"
       | otherwise = throwError $ "Can't encode char " <> show c

stringPtr :: Label -> CodeGen ()
stringPtr (Label l) = do
  ins $ "leaq " <> encodeUtf8 l <> "(%rip), %rax"
  ins "orq $3, %rax"

-- | Load a value from the given location.
load :: Location -> CodeGen ()
load (Closure i) = ins $ "movq " <> show ( 8 * i) <> "(%rdi), %rax"
load (Stack i)   = ins $ "movq " <> show (-8 * i) <> "(%rbp), %rax"
load (Param _)   = throwError "Can't load from parameter"

store :: Location -> CodeGen ()
store l@(Closure _) = storeVia "%rdi" l
store l@(Stack _)   = storeVia "%rbp" l
store l@(Param _)   = storeVia "%rsp" l

storeVia :: ByteString -> Location -> CodeGen ()
storeVia reg (Closure i) = ins $ "movq %rax, " <> show ( 8 * i) <> "(" <> reg <> ")"
storeVia reg (Stack i)   = ins $ "movq %rax, " <> show (-8 * i) <> "(" <> reg <> ")"
-- The first of the callee's stack slots is used for the return address, the
-- second for the callee's rbp-save, and so parameters start at stack slot 3.
storeVia reg (Param i)   = ins $ "movq %rax, " <> show (-8 * (i + 2)) <> "(" <> reg <> ")"

callClosure :: CodeGen ()
callClosure = do
  untagClosure
  ins "movq %rax, %rdi"
  ins "callq *(%rax)"

withSavedContext :: CodeGen () -> CodeGen ()
withSavedContext f = do
  ins "pushq %rdi"
  f
  ins "popq %rdi"

deref :: CodeGen ()
deref = ins "movq (%rax), %rax"

moveInt :: Int -> ByteString -> CodeGen ()
moveInt k target = ins $ "movq $" <> show k <> ", " <> target

define :: Text -> CodeGen ()
define name = do
  dir $ "globl " <> encodeUtf8 name
  dir "p2align 4, 0x90"
  label $ encodeUtf8 name

prologue :: Int -> CodeGen ()
prologue reservedWords = do
  define =<< context
  ins "pushq %rbp"
  ins "movq %rsp, %rbp"
  allocStack reservedWords

epilogue :: Int -> CodeGen ()
epilogue reservedWords = do
  freeStack reservedWords
  ins "popq %rbp"
  ins "retq"

tagClosure :: CodeGen ()
tagClosure = ins "orq $6, %rax"

untagClosure :: CodeGen ()
untagClosure = ins "subq $6, %rax"

tagChar :: CodeGen ()
tagChar = do
  ins "shlq $8, %rax"
  ins "orq $0xf, %rax"

untagChar :: CodeGen ()
untagChar = ins "shrq $8, %rax"

tagFixnum :: CodeGen ()
tagFixnum = ins "shl $2, %rax"

untagFixnum :: CodeGen ()
untagFixnum = ins "shr $2, %rax"

tagPair :: CodeGen ()
tagPair = ins "orq $1, %rax"

untagPair :: CodeGen ()
untagPair = ins "sub $1, %rax"

labelAddr :: Label -> CodeGen ()
labelAddr (Label l) = ins $ "leaq " <> encodeUtf8 l <> "(%rip), %rax"

string :: Text -> Label -> CodeGen ()
string s (Label l) = do
  setContext l
  define l
  dir $ "asciz \"" <> encodeUtf8 s <> "\""

alloc :: Alignment -> Int -> CodeGen ()
alloc a n = do
  align a
  ins "movq %rsi, %rax"
  ins $ "addq $" <> show (8 * n) <> ", %rsi"
  where
    align Align8 = pure ()
    align Align16 = do
      ins "addq $0x8, %rsi"
      ins "andq $0xfffffffffffffff0, %rsi"

box :: CodeGen ()
box = withComment "Box" $ do
  ins "pushq %rax"
  alloc Align8 1
  ins "popq %rdx"
  ins "movq %rdx, (%rax)"

callC :: Text -> CodeGen ()
callC sym = withComment ("Call foreign function " <> sym) $ do
  ins "pushq %rsi"      -- save heap ptr.
  ins "pushq %rdi"      -- save closure ptr.
  ins "movq %rax, %rdi" -- argument to function.

  ins "pushq %rbp"      -- align stack to 16 bytes.
  ins "movq %rsp, %rbp"
  ins "subq $8, %rsp"
  ins "andq $0xfffffffffffffff0, %rsp"

  ins $ "callq " <> encodeUtf8 sym

  ins "movq %rbp, %rsp" -- restore stack.
  ins "popq %rbp"

  ins "popq %rdi"       -- restore closure ptr.
  ins "popq %rsi"       -- restore heap ptr.

allocStack :: Int -> CodeGen ()
allocStack 0         = pure ()
allocStack wordCount = ins $ "subq $" <> bytes <> ", %rsp"
  where
    bytes = show $ wordCount * 8

freeStack :: Int -> CodeGen ()
freeStack 0         = pure ()
freeStack wordCount = ins $ "addq $" <> bytes <> ", %rsp"
  where
    bytes = show $ wordCount * 8

ins :: ByteString -> CodeGen ()
ins text = indent >> emit (text <> "\n")

sep :: CodeGen ()
sep = emit "\n"

dir :: ByteString -> CodeGen ()
dir name = indent >> emit ("." <> name <> "\n")

label :: ByteString -> CodeGen ()
label name = emit $ name <> ":\n"

comment :: Text -> CodeGen ()
comment text = indent >> emit ("; # " <> encodeUtf8 text <> "\n")

withComment :: Text -> CodeGen () -> CodeGen ()
withComment s m = do
  pushIndent
  comment s
  m
  popIndent

indent :: CodeGen ()
indent = emit =<< flip BS.replicate ' ' <$> getIndent
