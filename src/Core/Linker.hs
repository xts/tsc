module Core.Linker
  ( link
  ) where

import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(..))
import System.Process.Typed (readProcess, proc)
import System.IO.Temp (withSystemTempDirectory)

rtsMain :: ByteString
rtsMain = $(embedFile "src/Core/RTS/main.c")

rtsPrint :: ByteString
rtsPrint = $(embedFile "src/Core/RTS/print.c")

link :: ByteString -> String -> ExceptT String IO ()
link source out = do
  withSystemTempDirectory "tsc.work" $ \dir -> do
    let srcPath      = dir <> "/program.s"
        rtsMainPath  = dir <> "/main.c"
        rtsPrintPath = dir <> "/print.c"
    lift $ BS.writeFile srcPath source
    lift $ BS.writeFile rtsMainPath rtsMain
    lift $ BS.writeFile rtsPrintPath rtsPrint
    (rc, _, err) <- readProcess (proc "clang" [rtsMainPath, rtsPrintPath, srcPath, "-o", out])
    case rc of
      ExitSuccess   -> pure ()
      ExitFailure _ -> fail $ decodeUtf8 $ BS.toStrict err
