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
    map (reset . snd) <$> mapM (infer env) es

spec :: Spec
spec = do
  let t0 = TyVar 0
      t1 = TyVar 1

  describe "type inference" $ do

    it "infers booleans" $ do
      infer' "#t" `shouldBe` Right [TyBool]
      infer' "#f" `shouldBe` Right [TyBool]

    it "infers integers" $ do
      infer' "0" `shouldBe` Right [TyInt]
      infer' "1" `shouldBe` Right [TyInt]
      infer' "-1" `shouldBe` Right [TyInt]

    it "infers characters" $ do
      infer' "#\\a" `shouldBe` Right [TyChar]

    it "infers strings" $ do
      infer' "\"zebra\"" `shouldBe` Right [TyString]

    it "infers lists" $ do
      infer' "()" `shouldBe` Right [TyList t0]
      infer' "(list)" `shouldBe` Right [TyList t0]
      infer' "(list 1)" `shouldBe` Right [TyList TyInt]
      infer' "(cons 1 ())" `shouldBe` Right [TyList TyInt]
      infer' "(cdr ())" `shouldBe` Right [TyList t0]

    it "infers functions" $ do
      infer' "(lambda () #f)" `shouldBe` Right [TyFun [] TyBool]
      infer' "(lambda (x) x)" `shouldBe` Right [TyFun [t0] t0]
      infer' "(lambda (x y) y)" `shouldBe` Right [TyFun [t0, t1] t1]

    it "infers indefinite-arity functions" $ do
      infer' "(+)" `shouldBe` Right [TyInt]
      infer' "(+ 1)" `shouldBe` Right [TyInt]
      infer' "(+ 1 2)" `shouldBe` Right [TyInt]

    it "infers recursive functions" $ do
      infer' "(define (fib n) (+ (fib (- n 1)) (fib (- n 2)))) fib"
        `shouldBe` Right [TyFun [TyInt] TyInt]

    it "infers null?" $ do
      infer' (def_nullp <> "null?") `shouldBe` Right [TyFun [TyList t0] TyBool]

    it "infers map" $ do
      infer' (def_map <> "map")
        `shouldBe` Right [TyFun [TyFun [t0] t1, TyList t0] (TyList t1)]

    it "infers foldr" $ do
      infer' (def_foldr <> "fold-right")
        `shouldBe` Right [TyFun [TyFun [t0, t1] t1, t1, TyList t0] t1]

    it "infers foldl" $ do
      infer' (def_foldl <> "fold-left")
        `shouldBe` Right [TyFun [TyFun [t0, t1] t1, t1, TyList t0] t1]

    it "infers length" $ do
      infer' (def_length <> "length")
        `shouldBe` Right [TyFun [TyList t0] TyInt]

    it "infers insert" $ do
      infer' (def_insert <> "insert")
        `shouldBe` Right [TyFun [TyInt, TyList TyInt] (TyList TyInt)]

    it "infers sort" $ do
      infer' (def_sort <> "sort")
        `shouldBe` Right [TyFun [TyList TyInt] (TyList TyInt)]

def_nullp :: Text
def_nullp = "(define (null? xs) (eq xs ()))"

def_map :: Text
def_map = def_nullp <> "(define (map f xs) (if (null? xs) () (cons (f (car xs)) (map f (cdr xs)))))"

def_foldr :: Text
def_foldr = def_nullp <> "(define (fold-right f init xs) (if (null? xs) init (f (car xs) (fold-right f init (cdr xs)))))"

def_foldl :: Text
def_foldl = def_nullp <> "(define (fold-left f init xs) (if (null? xs) init (fold-left f (f (car xs) init) (cdr xs))))"

def_length :: Text
def_length = def_foldr <> "(define (length xs) (fold-right (lambda (x acc) (+ acc 1)) 0 xs))"

def_insert :: Text
def_insert = def_nullp
  <> "(define (insert x xs) (if (null? xs) (list x) (if (< (car xs) x) (cons (car xs) (insert x (cdr xs))) (cons x xs))))"

def_sort :: Text
def_sort = def_insert <> def_foldr <> "(define (sort xs) (fold-right insert () xs))"
