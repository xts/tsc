module Core.Compiler
  ( compile
  ) where

import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Core.CodeGen
import Core.Decomposer
import Core.Linker
import Core.Options
import Core.Parser
import Core.Prelude
import Core.Transform qualified as T
import Core.Transformers.Desugarer
import Core.Transformers.FreeFinder
import Core.Transformers.Lifter
import Core.Transformers.Renamer
import Core.Transformers.Resolver

compile :: Options -> IO (Either String ())
compile options = runExceptT $ do
  -- Read the source code.
  source <- case optSource options of
    Source text -> pure text
    File path   -> lift $ readFile path

  -- Apply our compilation pipeline with a finite number of credits. If we run
  -- out of credits, we abort and present the intermediate result to the user.
  asm <- T.evalTransform (optCredits options) $
    parse (optionalPrelude <> source)
     T.>>= desugar        -- Replace convenience forms with basic forms.
     T.>>= renameSymbols  -- Give each symbol a unique name, removing shadow bindings.
     T.>>= findFreeVars   -- Annotate each lambda with the free variables in its body.
     T.>>= resolveSymbols -- Resolve text symbols to typed memory locations.
     T.>>= liftPrimitives -- Lift primitives to closures where necessary.
     T.>>= decompose      -- Split the program into a list of functions and associated data.
     T.>>= lowerToAsm     -- Generate assembly code.
     T.>>= T.return       -- (Seemingly redundant, but spends a credit and enables asm output.)

  -- Link our program with the RTS to produce the final executable.
  link asm (optOut options)

  where
    optionalPrelude
      | optNoPrelude options = mempty
      | otherwise            = prelude
