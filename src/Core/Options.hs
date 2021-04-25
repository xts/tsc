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
  { optSource    :: Source
  , optOut       :: String
  , optCredits   :: Int
  , optNoPrelude :: Bool
  } deriving (Show)

defOptions :: Options
defOptions = Options (Source "") "a.out" maxBound False
