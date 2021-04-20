module Core.Transformers.FreeFinder where

import Data.Set qualified as Set

import Core.AST
import Core.CodeGen.Primitives

type Env = [Text]

-- | Annotate all lambda declarations with the free variables in their bodies.
findFree :: [Expr] -> Either String [Expr]
findFree = Right . runIdentity . traverseAst go
  where
    go (LamDef (Args as) _ es) = pure $ LamDef (Args as) (freeArgs as es) es
    go e                       = pure e

freeArgs :: Env -> [Expr] -> FreeArgs
freeArgs bns exs = FreeArgs $ map Name $ Set.toList $ execState (mapM (go bns) exs) mempty
  where
    go :: Env -> Expr -> State (Set Text) Expr
    -- Capture free valiables.
    go bs e@(Sym s) | isFree bs s = modify (Set.insert s) $> e

    -- Capture free variables from a deeper lambda that are also free in our context.
    go bs e@(LamDef _ (FreeArgs fs) _) =
      modify (Set.fromList (filter (isFree bs) (map name fs)) <>) $> e

    -- Recurse.
    go bs (If p t f)  = If <$> go bs p <*> go bs t <*> go bs f
    go bs (List es)   = List <$> mapM (go bs) es
    go bs (Let vs es) = do
      vs' <- mapM (\(Binding i@(Name s) e) -> Binding i <$> go (s:bs) e) vs
      es' <- mapM (go (map bindName vs' <> bs)) es
      pure $ Let vs' es'
    go _  e           = pure e

    -- A symbol is free if it is neither in our context nor a primitive.
    isFree bs s = not $ s `elem` bs || isPrimitive s

    bindName (Binding n _) = name n
    name (Name s)          = s
    name _                 = error "Id has no name; find free before resolving"
