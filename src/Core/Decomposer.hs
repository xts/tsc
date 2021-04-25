{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Decomposer
  ( Image(..)
  , Function(..)
  , decompose
  , mainLabel
  ) where

import Data.Map qualified as Map
import Data.Text qualified as T

import Core.IR
import Core.PrettyPrinter (HasPrettyPrint)
import Core.Transform (Transform, transform)

data Image = Image
  { imFuns :: [Function]
  , imStrs :: [(Text, Label)]
  } deriving (Eq, Show)

data Function = Function
  { funLabel :: Label
  , funArgs :: Args
  , funBody :: [Expr]
  } deriving (Eq, Show)

decompose :: Monad m => [Expr] -> Transform m Image
decompose es =
  let (es', strings) = extractStrings es
      (es'', lambdas)  = extractLambdas es'
      main = Function mainLabel (Args []) es''
  in transform $ Right $ Image (main : lambdas) $ Map.toList strings

extractLambdas :: [Expr] -> ([Expr], [Function])
extractLambdas es = runState (traverseIr go es) []
  where
    go :: Expr -> State [Function] Expr
    go (LamDef as fs es') = do
      labels <- get
      let lab = Label $ "_lambda_" <> show (length labels)
      modify (Function lab as es' :)
      pure $ LamDec as fs lab
    go e = pure e

extractStrings :: [Expr] -> ([Expr], Map Text Label)
extractStrings es = runState (traverseIr go es) mempty
  where
    go :: Expr -> State (Map Text Label) Expr
    go (Lit (String (Left s))) = Lit . String . Right <$> label s
    go e                       = pure e

    label :: Text -> State (Map Text Label) Label
    label s = get >>= \labels -> case Map.lookup s labels of
      Just l  -> pure l
      Nothing -> do
        let lab = Label $ "_string_" <> show (Map.size labels)
        modify $ Map.insert s lab
        pure lab

mainLabel :: Label
mainLabel = Label "_scheme_main"

instance HasPrettyPrint Image where
  prettyPrint (Image fs ss) = intercalate "\n\n" $ map prettyPrint fs ++ map prettyPrint ss

instance HasPrettyPrint Function where
  prettyPrint (Function (Label name) (Args as) body) =
    T.unpack name ++ ": (" ++ args ++ ")\n" ++ prettyPrint body
    where
      args = intercalate " " $ map T.unpack as

instance HasPrettyPrint (Text, Label) where
  prettyPrint (s, Label name) = T.unpack name ++ ": " ++ show s
