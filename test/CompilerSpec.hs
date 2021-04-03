module CompilerSpec where

import Data.Text (Text)
import Test.Hspec
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Char8 (strip)
import Data.ByteString qualified as BS
import System.Process.Typed (readProcess, proc)
import System.IO.Temp (withSystemTempFile)
import System.Exit (ExitCode(..))

import Core.Compiler
import Core.Options

running :: Text -> IO String
running source = withSystemTempFile "a.out" $ \path _ ->
  compile (defOptions { optSource = Source source, optOut = path }) >>= \case
    Left err -> pure err
    Right _  -> do
      (rc, stdout, _) <- readProcess (proc path [])
      case rc of
        ExitSuccess   -> pure $ toString $ strip $ BS.toStrict stdout
        ExitFailure _ -> pure "Failed to running program"

spec :: Spec
spec = do
  describe "compile" $ do
    -- Nil.
    it "compiles ()" $ do
      running "()" `shouldReturn` "()"

    -- Bool.
    it "compiles bool" $ do
      running "#t" `shouldReturn` "#t"
      running "#f" `shouldReturn` "#f"

    -- Fixnum.
    it "compiles fixnum" $ do
      running "0" `shouldReturn` "0"
      running "1" `shouldReturn` "1"
      running "1073741823" `shouldReturn` "-1"

{--
    -- String.
    it "compiles string" $ do
      running "\"\"" `shouldReturn` ""
      running "\"Hello\"" `shouldReturn` "Hello"
      running "\"hello, world\"" `shouldReturn` "hello, world"
--}
