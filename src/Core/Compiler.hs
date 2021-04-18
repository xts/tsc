module Core.Compiler
  ( compile
  ) where

import Control.Monad.Trans.Except (except, throwE)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Core.CodeGen
import Core.Linker
import Core.Options
import Core.Parser
import Core.Prelude
import Core.Transformer

compile :: Options -> IO (Either String ())
compile options = runExceptT pipeline
  where
    pipeline = do
      source <- case optSource options of
        Source text -> pure text
        File path   -> lift $ readFile path

      ast <- except $ parse $ if optNoPrelude options
        then source
        else prelude <> source
      maybeEmit optEmitAst $ show ast

      ast' <- except $ transform ast
      maybeEmit optEmitAst2 $ show ast'

      asm <- except $ lower ast'
      maybeEmit optEmitAsm $ decodeUtf8 asm

      link asm (optOut options)

    maybeEmit :: (Options -> Bool) -> String -> ExceptT String IO ()
    maybeEmit f x
      | f options = throwE x
      | otherwise = pure ()
