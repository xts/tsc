module Core.Decomposer
  ( Image(..)
  , decompose
  , mainLabel
  ) where

import Data.Map qualified as Map

import Core.IR
import Core.Extractor

data Image = Image
  { imFuns :: [Function]
  , imStrs :: [(Text, Label)]
  } deriving (Eq, Show)

decompose :: [Expr] -> Either String Image
decompose es =
  let (es', strings) = extractStrings es
      (es'', lambdas)  = extractLambdas es'
      main = Function mainLabel (Args []) es''
  in Right $ Image (main : lambdas) $ Map.toList strings

mainLabel :: Label
mainLabel = Label "_scheme_main"
