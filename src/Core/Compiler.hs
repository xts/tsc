module Core.Compiler
  ( compile
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Text.IO (readFile)
import Data.ByteString.UTF8 (toString)
import Prelude hiding (lex, readFile)

import Core.Analyser
import Core.CodeGen
import Core.Lexer
import Core.Parser
import Core.Options
import Core.Linker

compile :: Options -> IO (Either String ())
compile options = runExceptT pipeline
  where
    pipeline = do
      source <- case optSource options of
        Source text -> pure text
        File path   -> lift $ readFile path

      tokens <- except $ lex source
      ast    <- except $ parse tokens
      maybeEmit optEmitAst ast

      let ast2 = analyse ast
      maybeEmit optEmitAst2 ast2

      asm <- except $ lower ast2
      maybeEmit optEmitAsm asm


      link asm (optOut options)

    maybeEmit :: Show a => (Options -> Bool) -> a -> ExceptT String IO ()
    maybeEmit f x
      | f options = throwE $ show x
      | otherwise = pure ()
