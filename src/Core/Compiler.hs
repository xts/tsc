module Core.Compiler
  ( compile
  ) where

import Control.Monad.Trans.Except (except, throwE)
import Data.Text (unpack)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Core.AST qualified as AST
import Core.CodeGen
import Core.Decomposer
import Core.IR qualified as IR
import Core.Linker
import Core.Options
import Core.Parser
import Core.Prelude
import Core.Transformers.Desugarer
import Core.Transformers.FreeFinder
import Core.Transformers.Renamer
import Core.Transformers.Resolver

compile :: Options -> IO (Either String ())
compile options = runExceptT pipeline
  where
    pipeline = do
      -- Read the source code.
      source <- case optSource options of
        Source text -> pure text
        File path   -> lift $ readFile path

      -- Parse it, optionally with prelude.
      ast <- except $ parse $ optionalPrelude <> source
      maybeEmit optEmitAst $ AST.prettyPrint ast

      -- Transform it into a simpler form.
      ir <- except $
        desugar ast
        >>= rename
        >>= findFree
        >>= resolveSymbols
      maybeEmit optEmitIr $ IR.prettyPrint ir

      -- Decompose it into a series of functions and associated data.
      image <- except $ decompose ir

      -- Generate assembly.
      asm <- except $ lower image
      maybeEmit optEmitAsm $ decodeUtf8 asm

      -- Link executable.
      link asm (optOut options)

    maybeEmit f x
      | f options = throwE $ unpack x
      | otherwise = pure ()

    optionalPrelude
      | optNoPrelude options = mempty
      | otherwise            = prelude
