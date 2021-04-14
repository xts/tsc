module Core.CodeGen.Emitters
  ( ins
  , sep
  , dir
  , label
  , indent
  ) where

import Core.CodeGen.Monad

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
