module Core.Compiler
  ( compile
  ) where

import Control.Monad.Trans.Except (except, throwE)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Core.Analyser
import Core.CodeGen
import Core.Parser
import Core.Options
import Core.Renamer
import Core.Linker

compile :: Options -> IO (Either String ())
compile options = runExceptT pipeline
  where
    pipeline = do
      source <- case optSource options of
        Source text -> pure text
        File path   -> lift $ readFile path

      ast    <- except $ rename =<< parse source
      maybeEmit optEmitAst $ show ast

      let ast2 = analyse ast
      maybeEmit optEmitAst2 $ show ast2

      asm <- except $ lower ast2
      maybeEmit optEmitAsm $ decodeUtf8 asm

      link asm (optOut options)

    maybeEmit :: (Options -> Bool) -> String -> ExceptT String IO ()
    maybeEmit f x
      | f options = throwE x
      | otherwise = pure ()
