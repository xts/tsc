{-# OPTIONS_GHC -Wno-orphans #-}

module Core.PrettyPrinter
  ( PP
  , runPP
  , string
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

runPP :: PP -> Text
runPP f = execWriter $ runStateT f 0

intercalate :: PP -> [PP] -> PP
intercalate s = sequence_ . intersperse s

string :: Text -> PP
string s = modify (+ T.length s) >> tell s

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
