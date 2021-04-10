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
        ExitFailure _ -> pure "Failed to run program"

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

    -- Add.
    it "compiles arithmetic" $ do
      running "(print (+ 2 2))" `shouldReturn` "4"
      running "(print (+ -1 1))" `shouldReturn` "0"
      running "(print (- 2 2))" `shouldReturn` "0"
      running "(print (+ 2 (- 4 7)))" `shouldReturn` "-1"

    -- String.
    it "compiles string" $ do
      running "(print \"\")" `shouldReturn` ""
      running "(print \"Hello\")" `shouldReturn` "Hello"
      running "(print \"hello, world\")" `shouldReturn` "hello, world"

    -- If.
    it "compiles if" $ do
      running "(if #t (print 1) (print 0))" `shouldReturn` "1"
      running "(print (if #t 1 0))" `shouldReturn` "1"
      running "(print (if 0 1 0))" `shouldReturn` "1"
      running "(print (if \"\" 1 0))" `shouldReturn` "1"
      running "(print (if #f 1 0))" `shouldReturn` "0"
      running "(print (if #t 1))" `shouldReturn` "1"
      running "(if #f (print 0))" `shouldReturn` ""

    -- Less than.
    it "compiles less-than" $ do
      running "(print (< 1 2))" `shouldReturn` "#t"
      running "(print (< 2 1))" `shouldReturn` "#f"
      running "(print (< -1 0))" `shouldReturn` "#t"
      running "(print (< -1 -2))" `shouldReturn` "#f"

    -- Let.
    it "compiles let" $ do
      running "(print (let ((x 2)) (+ x x)))" `shouldReturn` "4"
      running "(print (let ((x 2) (y 1)) (+ x y)))" `shouldReturn` "3"
      running "(print (let (x) x))" `shouldReturn` "()"
      running "(print (let () 0))" `shouldReturn` "0"
      running "(print (let ((x 1)) (+ (let ((x 2)) x) x)))" `shouldReturn` "3"

    -- Expression sequences.
    it "compiles multiple expressions" $ do
      running "(print \"hello\") (print \"world\")" `shouldReturn` "hello\nworld"

    -- Rudimentary lambdas.
    it "compiles rudimentary lambdas" $ do
      running "((lambda () ((lambda () (print \"lam\")))))" `shouldReturn` "lam"
