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
  , deref
  , moveInt
  , define
  , prologue
  , epilogue
  , string
  , allocStack
  , freeStack
  , alloc
  , ins
  , sep
  , dir
  , label
  , indent
  ) where

import Control.Monad.Except (throwError)
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
store (Closure i) = ins $ "movq %rax, " <> show ( 8 * i) <> "(%rdi)"
store (Stack i)   = ins $ "movq %rax, " <> show (-8 * i) <> "(%rbp)"
-- The first of the callee's stack slots is used for the return address, the
-- second for the callee's rbp-save, and so parameters start at stack slot 3.
store (Param i)   = ins $ "movq %rax, " <> show (-8 * (i + 2)) <> "(%rsp)"

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
ins text = emit $ indent <> text <> "\n"

sep :: CodeGen ()
sep = emit "\n"

dir :: ByteString -> CodeGen ()
dir name = emit $ indent <> "." <> name <> "\n"

label :: ByteString -> CodeGen ()
label name = emit $ name <> ":\n"

indent :: ByteString
indent = "    "
