module Core.CodeGen
  ( lower
  ) where

import Data.Map qualified as Map
import Prelude hiding (State)

import Core.Analyser
import Core.Analyser.AST
import Core.CodeGen.Emitters
import Core.CodeGen.Expr
import Core.CodeGen.Monad
import Core.CodeGen.Primitives

lower :: ([Expr], Info) -> Either String ByteString
lower (es, info) = do
  main       <- snd <$> function "_scheme_entry" 0 [] es True
  lams       <- lambdas $ inLambdas info
  stringData <- snd <$> gen "strings" 0 [] (strings $ inStrings info)
  pure $ main <> lams <> stringData

gen :: Text -> Int -> [(Text, Var)] -> CodeGen () -> Either String (State, ByteString)
gen ctx nargs vars = runCodeGen ctx nargs vars primitives

lambdas :: [(Lambda, Label)] -> Either String ByteString
lambdas lams = mconcat . map snd <$> fns
  where fns = forM lams $ \(Lambda as fs e, lb) ->
          let vars = zip fs $ map Closure [1..]
          in function (unLabel lb) (length as) vars e False

strings :: Map Text Label -> CodeGen ()
strings labels = do
  forM_ (Map.toList labels) $ \(k, Label v) -> do
    sep
    dir $ "globl " <> encodeUtf8 v
    dir "p2align 4, 0x90"
    label $ encodeUtf8 v
    dir $ "asciz \"" <> encodeUtf8 k <> "\""

function :: Text -> Int -> [(Text, Var)] -> [Expr] -> Bool -> Either String (State, ByteString)
function ctx nargs vars es isMain = do
  (st, body) <- gen ctx nargs vars $ mapM_ expr es
  (_, pre)   <- gen ctx nargs [] $ prologue (stackSpace st) isMain
  (_, post)  <- gen ctx nargs [] $ epilogue (stackSpace st) isMain
  pure (st, pre <> body <> post)

prologue :: Int -> Bool -> CodeGen ()
prologue space isMain = do
  name <- encodeUtf8 <$> context
  dir "text"
  dir $ "globl " <> name
  dir "p2align 4, 0x90"
  label name
  ins "pushq %rbp"
  ins "movq %rsp, %rbp"
  when (space > 0) $
    ins $ "subq $" <> fromString (show space) <> ", %rsp"
  when isMain $ ins "movq %rdi, %rsi" -- Our argument is the heap ptr.

epilogue :: Int -> Bool -> CodeGen ()
epilogue space isMain = do
  when isMain $ ins "xorq %rax, %rax" -- Return 0 to the OS.
  when (space > 0) $
    ins $ "addq $" <> fromString (show space) <> ", %rsp"
  ins "popq %rbp"
  ins "retq"
