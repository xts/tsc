{-# OPTIONS_GHC -Wno-orphans #-}

module Core.PrettyPrinter
  ( PP
  , HasPrettyPrint(..)
  , runPP
  , string
  , spaced
  , intercalate
  , align
  , newline
  , (.+)
  , (.|)
  ) where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Text qualified as T
import Prelude hiding (intercalate)

type PP = StateT Int (Writer Text) ()

instance IsString PP where
  fromString s = string (T.pack s)

class HasPrettyPrint a where
  prettyPrint :: a -> String

runPP :: PP -> String
runPP f = T.unpack $ execWriter $ runStateT f 0

intercalate :: PP -> [PP] -> PP
intercalate s = sequence_ . intersperse s

string :: Text -> PP
string s = modify (+ T.length s) >> tell s

spaced :: (a -> PP) -> [a] -> PP
spaced f = intercalate " " . map f

newline :: PP
newline = put 0 >> tell "\n"

indent :: Int -> PP
indent n = string $ T.replicate n " "

align :: [PP] -> PP
align ps = do
  k <- get
  intercalate (newline >> indent k) ps
  put k

infixl 5 .+
(.+) :: PP -> PP -> PP
(.+) = (>>)

infix 4 .|
(.|) :: PP -> PP -> PP
(.|) a b = align [a, b]

instance HasPrettyPrint ByteString where
  prettyPrint = decodeUtf8
