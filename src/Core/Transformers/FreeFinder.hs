module Core.Transformers.FreeFinder where

import Core.AST
import Core.CodeGen.Primitives

type Env = [Text]

-- | Annotate all lambda declarations with the free variables in their bodies.
findFree :: [Expr] -> Either String [Expr]
findFree = Right . runIdentity . traverseAst go
  where
    go (LamDef (Args as) _ es) = pure $ LamDef (Args as) (Just $ freeArgs as es) es
    go e                       = pure e

freeArgs :: Env -> [Expr] -> Args
freeArgs bns exs = Args $ execState (mapM (go bns) exs) []
  where
    go :: Env -> Expr -> State [Text] Expr
    -- Capture free valiables.
    go bs e@(Sym s) | isFree bs s = modify (s:) $> e

    -- Capture free variables from a deeper lambda that are also free in our context.
    go bs e@(LamDef _ (Just (Args fs)) _) = modify (filter (isFree bs) fs <>) $> e

    -- Recurse.
    go bs (If p t f)  = If <$> go bs p <*> go bs t <*> go bs f
    go bs (List es)   = List <$> mapM (go bs) es
    go bs (Let vs es) = do
      vs' <- mapM (\(Binding s e) -> Binding s <$> go (s:bs) e) vs
      es' <- mapM (go (map bName vs' <> bs)) es
      pure $ Let vs' es'
    go _  e           = pure e

    -- A symbol is free if it is neither in our context nor a primitive.
    isFree bs s = not $ s `elem` bs || isPrimitive s
