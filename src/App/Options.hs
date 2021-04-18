module Options
  ( parseOptions
  ) where

import Options.Applicative

import Core.Options

options :: Parser Options
options = Options
  <$> argument (File <$> str) (metavar "SOURCE" <> help "Source file")
  <*> strOption (short 'o' <> value "a.out" <> metavar "OUT" <> help "Output executable")
  <*> switch (long "emit-ast" <> help "Print parser AST and exit")
  <*> switch (long "emit-ast2" <> help "Print analyser AST and exit")
  <*> switch (long "emit-asm" <> help "Print assembly and exit")
  <*> switch (long "no-prelude" <> help "Do not inject prelude")

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
  (fullDesc <> progDesc "Compile a scheme program" <> header "Scheme compiler")
