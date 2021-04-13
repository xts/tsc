module Core.Linker
  ( link
  ) where

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(..))
import System.Process.Typed (readProcess, proc)
import System.IO.Temp (withSystemTempDirectory)

rts :: ByteString
rts = $(embedFile "src/Core/rts.c")

link :: ByteString -> String -> ExceptT String IO ()
link source out = do
  withSystemTempDirectory "tsc.work" $ \dir -> do
    let srcPath = dir <> "/program.s"
        rtsPath = dir <> "/rts.c"
    lift $ BS.writeFile srcPath source
    lift $ BS.writeFile rtsPath rts
    (rc, _, err) <- readProcess (proc "clang" [rtsPath, srcPath, "-o", out])
    case rc of
      ExitSuccess   -> pure ()
      ExitFailure _ -> fail $ decodeUtf8 $ BS.toStrict err
