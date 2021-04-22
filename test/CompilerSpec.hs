module CompilerSpec where

import Test.Hspec
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
      (rc, out, _) <- readProcess (proc path [])
      case rc of
        ExitSuccess   -> pure $ decodeUtf8 $ strip $ BS.toStrict out
        ExitFailure _ -> pure "Failed to run program"

spec :: Spec
spec = parallel $ do
  describe "compile" $ do
    -- Nil.
    it "compiles ()" $ do
      running "(display ())" `shouldReturn` "()"

    -- Bool.
    it "compiles bool" $ do
      running "(display #t)" `shouldReturn` "#t"
      running "(display #f)" `shouldReturn` "#f"

    -- Fixnum.
    it "compiles fixnum" $ do
      running "(display 0)" `shouldReturn` "0"
      running "(display 1)" `shouldReturn` "1"
      running "(display -1)" `shouldReturn` "-1"
      running "(display -536870912)" `shouldReturn` "-536870912"
      running "(display 536870911)" `shouldReturn` "536870911"

    -- Char.
    it "compiles char" $ do
      running "(display #\\a)" `shouldReturn` "a"
      running "(display #\\Z)" `shouldReturn` "Z"
      running "(display #\\0)" `shouldReturn` "0"

    -- Add.
    it "compiles arithmetic" $ do
      running "(display (+ 2 2))" `shouldReturn` "4"
      running "(display (+ -1 1))" `shouldReturn` "0"
      running "(display (- 2 2))" `shouldReturn` "0"
      running "(display (+ 2 (- 4 7)))" `shouldReturn` "-1"

    -- String.
    it "compiles string" $ do
      running "(display \"\")" `shouldReturn` ""
      running "(display \"Hello\")" `shouldReturn` "Hello"
      running "(display \"hello, world\")" `shouldReturn` "hello, world"

    -- If.
    it "compiles if" $ do
      running "(if #t (display 1) (display 0))" `shouldReturn` "1"
      running "(display (if #t 1 0))" `shouldReturn` "1"
      running "(display (if 0 1 0))" `shouldReturn` "1"
      running "(display (if \"\" 1 0))" `shouldReturn` "1"
      running "(display (if #f 1 0))" `shouldReturn` "0"
      running "(display (if #t 1))" `shouldReturn` "1"
      running "(if #f (display 0))" `shouldReturn` ""

    -- Less than.
    it "compiles less-than" $ do
      running "(display (< 1 2))" `shouldReturn` "#t"
      running "(display (< 2 1))" `shouldReturn` "#f"
      running "(display (< -1 0))" `shouldReturn` "#t"
      running "(display (< -1 -2))" `shouldReturn` "#f"
      running "(display (< 1 1))" `shouldReturn` "#f"

    -- Let.
    it "compiles let" $ do
      running "(display (let ((x 2)) (+ x x)))" `shouldReturn` "4"
      running "(display (let ((x 2) (y 1)) (+ x y)))" `shouldReturn` "3"
      running "(display (let (x) x))" `shouldReturn` "()"
      running "(display (let () 0))" `shouldReturn` "0"
      running "(display (let ((x 1)) (+ (let ((x 2)) x) x)))" `shouldReturn` "3"
      running "(let ((say (lambda (x) (display x)))) (say \"hi\") (say \"you\"))" `shouldReturn` "hiyou"

    -- Expression sequences.
    it "compiles multiple expressions" $ do
      running "(display \"hello\") (display \"world\")" `shouldReturn` "helloworld"

    -- Rudimentary lambdas.
    it "compiles rudimentary lambdas" $ do
      running "((lambda () ((lambda () (display \"lam\")))))" `shouldReturn` "lam"

    -- Lambdas with parameters.
    it "compiles non-closure lambdas" $ do
      running "(display ((lambda (x y) (+ x y)) 42 9))" `shouldReturn` "51"
      running "(let ((say (lambda (x) (display x)))) (say \"hi\"))" `shouldReturn` "hi"

    -- Fibonacci.
    it "compiles self-unaware fibonacci" $ do
      running "(let ((fib (lambda (fib k) (if (< k 3) 1 (+ (fib fib (- k 1)) (fib fib (- k 2))))))) (display (fib fib 30)))"
        `shouldReturn` "832040"

    it "compiles fibonacci" $ do
      running "(let ((fib (lambda (k) (if (< k 3) 1 (+ (fib (- k 1)) (fib (- k 2))))))) (display (fib 30)))"
        `shouldReturn` "832040"

    -- Renamer.
    it "compiles closures without overriding shadow bindings" $ do
      running "(display ((lambda (x) (let ((x #t)) x)) #f))" `shouldReturn` "#t"

    -- Closures.
    it "compiles closures and captures static values" $ do
      running "(let ((f (lambda () (let ((x 42)) (lambda () x))))) (display ((f))))" `shouldReturn` "42"

    -- set!
    it "compiles destructive updates" $ do
      running "(let ((x 0)) (set! x 42) (display x))" `shouldReturn` "42"
      running "(let ((count (let ((c 0)) (lambda () (set! c (+ 1 c)) c)))) (display (count)) (display (count)) (display (count)))"
        `shouldReturn` "123"

    -- Nested closure calls.
    it "restores closures" $ do
      running "(let ((x 0)) (let ((inc (lambda () (set! x (+ x 1))))) (let ((inc2 (lambda () (inc) (inc)))) (inc2) (display x))))"
        `shouldReturn` "2"
