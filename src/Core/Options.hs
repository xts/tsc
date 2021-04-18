module Core.Options
  ( Options(..)
  , Source(..)
  , defOptions
  ) where

data Source
  = Source Text
  | File String
  deriving (Show)

data Options = Options
  { optSource   :: Source
  , optOut      :: String
  , optEmitAst  :: Bool
  , optEmitAst2 :: Bool
  , optEmitAsm  :: Bool
  , optNoPrelude :: Bool
  } deriving (Show)

defOptions :: Options
defOptions = Options (Source "") "a.out" False False False False
