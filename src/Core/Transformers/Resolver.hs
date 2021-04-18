module Core.Transformers.Resolver
  ( resolveSymbols
  ) where

import Data.Map qualified as Map

import Core.AST
import Core.CodeGen.Primitives

type Res = (Text, Expr)

resolveSymbols :: [Expr] -> Either String [Expr]
resolveSymbols = Right . map (resolve prims)
  where
    prims = map (\s -> (s, Prim s)) $ Map.keys primitives

resolve :: [Res] -> Expr -> Expr

resolve rs e@(Sym s) = case find ((==s) . fst) rs of
  Just (_, loc) -> loc
  Nothing       -> e

resolve rs (Let vs es) =
  let vs' = map (\(Binding s e) -> Binding s (resolve ((s, Sym s) : rs) e)) vs
      es' = map (resolve (map (\(Binding s _) -> (s, Sym s)) vs <> rs)) es
  in Let vs' es'

resolve rs (LamDef (Args as) (Just (Args fs)) es) =
  let enumWith c xs = zipWith (\x n -> (x, c n)) xs [1..]
      as' = enumWith Arg as
      fs' = enumWith CArg fs
  in LamDef (Args as) (Just $ Args fs) $ map (resolve $ as' <> fs' <> rs) es

resolve rs (List es)  = List $ map (resolve rs) es
resolve rs (If p t f) = If (resolve rs p) (resolve rs t) (resolve rs f)

resolve _ e = e
