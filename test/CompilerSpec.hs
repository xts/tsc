module CompilerSpec where

import Test.Hspec
import Data.ByteString.Char8 (strip)
import Data.ByteString qualified as BS
import System.Process.Typed (readProcess, proc)
import System.Directory (listDirectory)
import System.IO.Temp (withSystemTempFile)
import System.FilePath (dropExtension, takeExtension)
import System.Exit (ExitCode(..))

import Core.Compiler
import Core.Options

running :: Text -> IO String
running source = withSystemTempFile "a.out" $ \path _ ->
  compile (defOptions { optSource = Source source, optOut = path }) >>= \case
    Left err -> pure err
    Right _  -> do
      (rc, out, _) <- readProcess (proc path [])
      case rc of
        ExitSuccess   -> pure $ decodeUtf8 $ strip $ BS.toStrict out
        ExitFailure _ -> pure "Failed to run program"

findTests :: IO [(String, Text, String)]
findTests = do
  testNames <- sort . map dropExtension . filter isTest <$> listDirectory base
  forM testNames $ \name -> do
    source <- strip <$> BS.readFile (base ++ name ++ ".scm")
    result <- strip <$> BS.readFile (base ++ name ++ ".exp")
    pure (name, decodeUtf8 source, decodeUtf8 result)
  where
    base = "test/cases/"
    isTest file = takeExtension file == ".scm"

spec :: Spec
spec = do
  tests <- runIO $ findTests
  parallel $ do
    describe "compile" $ do
      forM_ tests $ \(name, program, result) -> do
        it (map replaceUnderscore name) $ do
          running program `shouldReturn` result

  where
    replaceUnderscore '_' = ' '
    replaceUnderscore c   = c
