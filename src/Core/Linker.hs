module Core.Linker
  ( link
  ) where

import Data.ByteString qualified as BS
import Data.FileEmbed (embedDir)
import Prelude hiding (writeFile)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension)
import System.Process.Typed (readProcess, proc)
import System.IO.Temp (withSystemTempDirectory)

rts :: [(FilePath, ByteString)]
rts = $(embedDir "src/Core/RTS")

link :: ByteString -> String -> ExceptT String IO ()
link source out = do
  withSystemTempDirectory "tsc.work" $ \dir -> do
    -- Write all sources to disk.
    let files = map (first ((dir <> "/") <>)) $ ("program.s", source) : rts
    forM_ files $ \(path, content) -> lift $ BS.writeFile path content

    -- Ask clang to link them.
    let sources = filter ((/= ".h") . takeExtension) $ map fst files
    (rc, _, err) <- readProcess $ proc "clang" ("-o" : out : sources)
    case rc of
      ExitSuccess   -> pure ()
      ExitFailure _ -> fail $ decodeUtf8 $ BS.toStrict err
