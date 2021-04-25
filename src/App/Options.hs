module Options
  ( parseOptions
  ) where

import Options.Applicative

import Core.Options

options :: Parser Options
options = Options
  <$> argument (File <$> str) (metavar "SOURCE" <> help "Source file")
  <*> strOption (short 'o' <> value "a.out" <> metavar "OUT" <> help "Output executable")
  <*> option auto (short 'e'
                   <> long "emit-ir"
                   <> value maxBound
                   <> metavar "N"
                   <> help "Print intermediate result from stage N and exit")
  <*> switch (long "no-prelude" <> help "Do not inject prelude")

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
  (fullDesc <> progDesc "Compile a scheme program" <> header "Scheme compiler")
