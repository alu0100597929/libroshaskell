module MaybeMonad where

import Prelude hiding (Maybe, Nothing, Just)

data Maybe x = Nothing | Just x

{-
nuestras herramientas son
Just :: a -> Maybe a
Nothing :: Maybe a
y el pattern matching
-}

-- fmap :: (Functor f) => (a -> b) -> f a -> f b
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x) 

--pure :: a -> f a
--(<*>) :: f (a -> b) -> f a -> f b
instance Applicative Maybe where
  pure = Just

  Nothing <*> _ = Nothing
  (Just f) <*> v = fmap f v

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad Maybe where
  return a = Just a

  (Just x) >>= f = f x
  Nothing >>= _ = Nothing