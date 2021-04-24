module Main where

import Options
import Core.Compiler

main :: IO ()
main = do
  options <- parseOptions
  compile options >>= \case
    Left err  -> putStrLn err >> exitFailure
    Right _   -> pure ()
