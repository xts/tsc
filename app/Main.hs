{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compiler
import Data.ByteString qualified as BS
import Data.Text.IO (readFile)
import Linker
import Parser
import Options
import Prelude hiding (readFile)
import System.Exit (exitFailure)

main :: IO ()
main = do
  options <- parseOptions
  source <- readFile (optSource options)
  case parse source of
    Left err  -> do
      putStrLn err
      exitFailure
    Right ast -> do
      if (optEmitAst options)
        then putStrLn $ show ast
        else do
          let asm = compile ast
          if (optEmitAsm options)
            then BS.putStr asm
            else link asm (optOut options) >>= \case
              True -> pure ()
              False -> exitFailure
