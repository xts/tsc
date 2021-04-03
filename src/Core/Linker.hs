module Core.Linker
  ( link
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 (toString)
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
    (rc, _, stderr) <- readProcess (proc "clang" [rtsPath, srcPath, "-o", out])
    case rc of
      ExitSuccess   -> pure ()
      ExitFailure _ -> fail $ toString $ BS.toStrict stderr
