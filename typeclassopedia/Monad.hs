-- Monad.hs

import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Just a
             | Nothing
             deriving Show

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
  pure = Just

  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just g) <*> (Just value) = Just $ g value

instance Monad Maybe where
  return = pure

  Nothing >>= _ = Nothing
  (Just value) >>= g = g value