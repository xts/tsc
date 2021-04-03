module Core.Options
  ( Options(..)
  , Source(..)
  , defOptions
  ) where

import Data.Text (Text)

data Source
  = Source Text
  | File String
  deriving (Show)

data Options = Options
  { optSource  :: Source
  , optOut     :: String
  , optEmitAst :: Bool
  , optEmitAsm :: Bool
  } deriving (Show)

defOptions :: Options
defOptions = Options (Source "") "a.out" False False
