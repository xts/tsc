module Core.Options
  ( Options(..)
  ) where

data Options = Options
  { optSource  :: String
  , optOut     :: String
  , optEmitAst :: Bool
  , optEmitAsm :: Bool
  } deriving (Show)
