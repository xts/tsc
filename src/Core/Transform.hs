module Core.Transform where

import Control.Monad.Except (liftEither, throwError)
import Prelude hiding (return, (>>=), (>>))

import Core.PrettyPrinter (HasPrettyPrint(..))

-- | The Transform monad adds a counter to an ExceptT.
--
-- The bind operator decrements the counter and terminates the chain of monadic
-- actions when the counter hits zero by throwing the last result. This allows
-- us to chain program transformations and display any intermediate result.
--
-- Our intermediate results are restricted to HasPrettyPrint which means
-- Transform cannot be a Monad; use with QualifiedDo.
newtype Monad m => Transform m a =
  Transform { runTransform :: Int -> ExceptT String m (a, Int) }

-- | Evaluate at most `s` monadic steps, throwing the intermediate result at
-- step `s` if we hit the limit. Otherwise computes to completion.
evalTransform :: Monad m => Int -> Transform m a -> ExceptT String m a
evalTransform s m = do
  (a, _) <- runTransform m s
  pure a

-- | Our faux-return.
return :: Monad m => a -> Transform m a
return a = Transform $ \s -> pure (a, s)

-- | Our faux-bind, throwing when we run out of credits.
(>>=) :: (HasPrettyPrint a, Monad m) => Transform m a -> (a -> Transform m b) -> Transform m b
m >>= k = Transform $ \s -> do
  (a, s') <- runTransform m s
  if s' == 0
    then throwError (prettyPrint a)
    else runTransform (k a) (pred s')

-- | Our faux-bind, throwing when we run out of credits.
(>>) :: (HasPrettyPrint a, Monad m) => Transform m a -> Transform m b -> Transform m b
m >> k = m >>= (\_ -> k)

-- | Create a Transform action out of an Either.
transform :: Monad m => Either String a -> Transform m a
transform m = Transform $ \s -> do
  a <- liftEither m
  pure (a, s)
