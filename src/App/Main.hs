module Main where

import Options
import Core.Compiler

main :: IO ()
main = parseOptions >>= compile >>= \case
  Left err  -> putStrLn err >> exitFailure
  Right _   -> pure ()
