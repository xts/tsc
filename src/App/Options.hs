module Options
  ( parseOptions
  ) where

import Options.Applicative

import Core.Options

options :: Parser Options
options = Options
  <$> argument str (help "Source file")
  <*> strOption (short 'o' <> value "a.out" <> help "Output file")
  <*> switch (long "emit-ast" <> help "Emit AST")
  <*> switch (long "emit-asm" <> help "Emit assembly")

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
  (fullDesc <> progDesc "Compile a scheme program" <> header "header?")
