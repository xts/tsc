module Core.CodeGen.Emitters
  ( Location(..)
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
  , heapAlign16
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

load :: Location -> CodeGen ()
load (Stack i) = ins $ "movq " <> show (-8 * i) <> "(%rbp), %rax"
load (Closure i) = ins $ "movq " <> show (8 * i) <> "(%rdi), %rax"

store :: Location -> CodeGen ()
store (Param i)   = ins $ "movq %rax, " <> show (-8 * (i + 2)) <> "(%rsp)"
store (Stack i)   = ins $ "movq %rax, " <> show (-8 * i) <> "(%rbp)"
store (Closure i) = ins $ "movq %rax, " <> show (8 * i) <> "(%rdi)"

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

heapAlign16 :: CodeGen ()
heapAlign16 = do
  ins "movq %rsi, %rax"
  ins "andq $0xf, %rax"
  ins "addq $0x10, %rsi"
  ins "subq %rax, %rsi"

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
