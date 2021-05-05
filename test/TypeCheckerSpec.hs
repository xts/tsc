module TypeCheckerSpec where

import Control.Monad.Except (runExcept)
import Test.Hspec

import Core.Parser
import Core.Transform qualified as T
import Core.Transformers.Desugarer
import Core.Transformers.Renamer
import Core.TypeChecker
import Core.TypeChecker.Types
import Prelude hiding (Type)

infer' :: Text -> Either String [Type]
infer' s = runExcept $ do
  es <- T.evalTransform 3 $ (parse s T.>>= desugar T.>>= renameSymbols)
  flip evalStateT 0 $ do
    env <- primitiveEnv
    map snd <$> mapM (infer env) es

spec :: Spec
spec = do
  describe "infer" $ do
    pure ()

    {-
"(define (null? xs) (eq xs ()))"
"(define (map f xs) (if (null? xs) () (cons (f (car xs)) (map f (cdr xs)))))"
"(define (fold-right f init xs) (if (null? xs) init (f (car xs) (fold-right f init (cdr xs)))))"
"(define (fold-left f init xs) (if (null? xs) init (fold-left f (f (car xs) init) (cdr xs))))"
"(define (length xs) (fold-right (lambda (x acc) (+ acc 1)) 0 xs))"
"(define (insert x xs) (if (null? xs) (list x) (if (< (car xs) x) (cons (car xs) (insert x (cdr xs))) (cons x xs))))"
"(define (sort xs) (fold-right insert () xs))"
-}
