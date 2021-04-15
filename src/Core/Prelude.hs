module Core.Prelude
  ( prelude
  ) where

import Data.FileEmbed (embedFile)

prelude :: Text
prelude = decodeUtf8 $(embedFile "src/Core/Prelude/prelude.scm")
