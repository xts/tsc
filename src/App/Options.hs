module Options
  ( parseOptions
  ) where

import Options.Applicative

import Core.Options

options :: Parser Options
options = Options
  <$> argument (File <$> str) (metavar "SOURCE" <> help "Source file")
  <*> strOption (short 'o' <> value "a.out" <> metavar "OUT" <> help "Output executable")
  <*> switch (long "emit-ast" <> help "Print AST and exit")
  <*> switch (long "emit-asm" <> help "Print assembly and exit")

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
  (fullDesc <> progDesc "Compile a scheme program" <> header "Scheme compiler")
