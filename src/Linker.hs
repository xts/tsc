module Linker
  ( link
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import System.Exit (ExitCode(..))
import System.Process.Typed (readProcess, proc)
import Prelude hiding (writeFile)

rts :: ByteString
rts = $(embedFile "src/rts.c")

link :: ByteString -> String -> IO Bool
link source out = do
  BS.writeFile "/tmp/program.s" source
  BS.writeFile "/tmp/rts.c" rts
  (rc, stdout, stderr) <- readProcess (proc "clang" ["/tmp/rts.c", "/tmp/program.s", "-o", out])
  case rc of
    ExitSuccess   -> pure True
    ExitFailure _ -> (BS.putStr $ BS.toStrict stderr) >> pure False
