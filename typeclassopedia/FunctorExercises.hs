module FunctorExercises where

--Implement Functor instances for Either e and ((->) e).

instance Functor (Either e) where
  fmap _ (Left a) = (Left a)
  fmap g (Right b) = Right (g b)

instance Functor ((->) r) where
  fmap = (.)

data Pair a = Pair a a