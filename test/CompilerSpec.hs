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
      running "(print ())" `shouldReturn` "()"

    -- Bool.
    it "compiles bool" $ do
      running "(print #t)" `shouldReturn` "#t"
      running "(print #f)" `shouldReturn` "#f"

    -- Fixnum.
    it "compiles fixnum" $ do
      running "(print 0)" `shouldReturn` "0"
      running "(print 1)" `shouldReturn` "1"
      running "(print -1)" `shouldReturn` "-1"
      running "(print -536870912)" `shouldReturn` "-536870912"
      running "(print 536870911)" `shouldReturn` "536870911"

    -- Char.
    it "compiles char" $ do
      running "(print #\\a)" `shouldReturn` "a"
      running "(print #\\Z)" `shouldReturn` "Z"
      running "(print #\\0)" `shouldReturn` "0"

{--
    -- String.
    it "compiles string" $ do
      running "\"\"" `shouldReturn` ""
      running "\"Hello\"" `shouldReturn` "Hello"
      running "\"hello, world\"" `shouldReturn` "hello, world"
--}
