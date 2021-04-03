module Core.Compiler
  ( compile
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Text.IO (readFile)
import Data.ByteString.UTF8 (toString)
import Prelude hiding (readFile)

import Core.CodeGen
import Core.Parser
import Core.Options
import Core.Linker

compile :: Options -> IO (Either String ())
compile options = runExceptT pipeline
  where
    pipeline :: ExceptT String IO ()
    pipeline = do
      source <- case optSource options of
        Source text -> pure text
        File path -> lift $ readFile path

      ast <- except $ parse source
      maybeEmitAst ast

      asm <- except $ lower ast
      maybeEmitAsm asm

      link asm (optOut options)

    maybeEmitAst ast
      | optEmitAst options = throwE $ show ast
      | otherwise          = pure ()

    maybeEmitAsm asm
      | optEmitAsm options = throwE $ toString asm
      | otherwise          = pure ()
