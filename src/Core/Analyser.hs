module Core.Analyser
  ( Labels(..)
  , Label(..)
  , analyse
  ) where

import Control.Monad.State (State, runState, get, modify)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)

import Core.Analyser.AST
import Core.Parser.AST qualified as P

newtype Labels = Labels
  { lbStrings :: Map Text Label
  } deriving (Show)

type Analyser a = State Labels a

analyse :: [P.Expr] -> ([Expr], Labels)
analyse es = runState (mapM expr es) $ Labels Map.empty

expr :: P.Expr -> Analyser Expr
expr P.Nil       = pure Nil
expr (P.Sym s)   = pure $ Sym s
expr (P.List xs) = List <$> mapM expr xs
expr (P.Lit lit) = literal lit

literal :: P.Literal -> Analyser Expr
literal (P.Bool b)   = pure $ Lit $ Bool b
literal (P.Char c)   = pure $ Lit $ Char c
literal (P.Fixnum k) = pure $ Lit $ Fixnum k
literal (P.String s) = Lit . String <$> stringLabel s

stringLabel :: Text -> Analyser Label
stringLabel text = do
  strings <- lbStrings <$> get
  case Map.lookup text strings of
    Just lb -> pure lb
    Nothing -> do
      let lb = Label $ "_string_" <> pack (show $ Map.size strings)
      modify $ \st -> st { lbStrings = Map.insert text lb strings }
      pure lb
