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

-- | An action in our pretty printer monad. Holds the current column.
type PP = StateT Int (Writer Text) ()

-- | Allow use of literals.
instance IsString PP where
  fromString s = string (T.pack s)

-- | Using our own composition operator is both clearer and resolves ambiguity
-- with overloaded strings.
infixl 5 .+
(.+) :: PP -> PP -> PP
(.+) = (>>)

-- | The alignment operator can be used to more clearly signal layout. E.g.
--
--     "relative to this text"
--  .| "  " .+ "this text is indented by two columns."
infix 4 .|
(.|) :: PP -> PP -> PP
(.|) a b = align [a, b]

-- | Run a pretty print action.
runPP :: PP -> String
runPP f = T.unpack $ execWriter $ runStateT f 0

-- | Apply the provided list of actions, separated by applications of `s`.
intercalate :: PP -> [PP] -> PP
intercalate s = sequence_ . intersperse s

-- | Print a string.
string :: Text -> PP
string s = modify (+ T.length s) >> tell s

-- | Transform a list to pretty print actions and apply them with a space between each.
spaced :: (a -> PP) -> [a] -> PP
spaced f = intercalate " " . map f

-- | Print a newline.
newline :: PP
newline = put 0 >> tell "\n"

-- | Indent the current line by `n` columns.
indent :: Int -> PP
indent n = string $ T.replicate n " "

-- | Apply a sequence of pretty print actions such that the first is printed
-- at the current column, and each subsequent action prints at the same
-- column of the following line.
align :: [PP] -> PP
align ps = do
  k <- get
  intercalate (newline >> indent k) ps
  put k

-- | Provide a common constraint..
class HasPrettyPrint a where
  prettyPrint :: a -> String

-- Implement prettyPrint for common classes.
instance HasPrettyPrint ByteString where
  prettyPrint = decodeUtf8
