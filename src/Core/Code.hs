module Core.Code
  ( Code(..)
  , toEither
  , ins
  , dir
  , label
  ) where

import Data.ByteString (ByteString)

data Code
  = Code ByteString
  | Error String

instance Semigroup Code where
  (<>) = mappend

instance Monoid Code where
  mempty = Code mempty
  Error e  `mappend` _       = Error e
  Code _   `mappend` Error e = Error e
  Code a   `mappend` Code b  = Code (a <> b)

toEither :: Code -> Either String ByteString
toEither (Code c)  = Right c
toEither (Error e) = Left e

ins :: ByteString -> Code
ins text = Code $ indent <> text <> "\n"

dir :: ByteString -> Code
dir name = Code $ indent <> "." <> name <> "\n"

label :: ByteString -> Code
label sym = Code $ sym <> ":\n"

indent :: ByteString
indent = "    "
