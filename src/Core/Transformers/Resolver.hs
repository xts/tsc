module Core.Transformers.Resolver
  ( resolveSymbols
  ) where

import Data.Map qualified as Map

import Core.AST qualified as A
import Core.IR
import Core.CodeGen.Primitives

type Res = (Text, Expr)

-- | Translate from AST to IR, determining memory locations and replacing text
-- symbols with typed indices.
resolveSymbols :: [A.Expr] -> Either String [Expr]
resolveSymbols es = Right $ map (resolve prims) es
  where
    prims = map (\s -> (s, Prim s)) $ Map.keys primitives

resolve :: [Res] -> A.Expr -> Expr

-- Replace symbols with their resolutions.
resolve rs (A.Sym s) = resolution rs s

-- Resolve let bindings to Var expressions.
resolve rs (A.Let vs es) =
  let vars = zipWith (\b i -> (A.bName b, Var i)) vs [nextVar rs..]
      vs'  = zipWith (\r@(_, Var i) b -> Binding i $ resolve (r : rs) (A.bVal b)) vars vs
  in Let vs' $ map (resolve $ vars <> rs) es

-- Resolve arguments to Args and free variables to CArgs (closure arguments.)
resolve rs (A.LamDef (A.Args as) (A.FreeArgs fs) es) =
  let enumWith c xs = zipWith (\x n -> (x, c n)) xs [1..]
      as'  = enumWith Arg as
      fs'  = enumWith CArg fs
      free = FreeArgs $ map (resolutionVar rs) fs
      -- Insert a sentinel to evaluate the body with a fresh let binding stack.
      body = map (resolve $ as' <> fs' <> (sentinel : rs)) es
  in LamDef (Args as) free body

-- Recurse.
resolve rs (A.List es)  = List $ map (resolve rs) es
resolve rs (A.If p t f) = If (resolve rs p) (resolve rs t) (resolve rs f)
resolve _ (A.Lit x)     = Lit x
resolve _ A.Nil         = Nil
resolve _ e             = error $ "Cannot resolve-transform " <> show e

resolution :: [Res] -> Text -> Expr
resolution rs s = case find ((== s) . fst) rs of
  Just (_, e) -> e
  _           -> error $ "failed to resolve " <> show s

resolutionVar :: [Res] -> Text -> Int
resolutionVar rs s = case resolution rs s of
  Var i -> i
  _     -> error $ "not a variable " <> show s

nextVar :: [Res] -> Int
nextVar ((_, Var k) : _) = succ k
nextVar (e          : es)
  | e == sentinel = 1
  | otherwise     = nextVar es
nextVar []        = 1

sentinel :: Res
sentinel = (mempty, Nil)
