import Prelude hiding (Maybe, Nothing, Just)

data Maybe x = Nothing | Just x

{-
nuestras herramientas son
Just :: a -> Maybe a
Nothing :: Maybe a
y el pattern matching
-}

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x) 

instance Applicative Maybe where
  pure = Just

  Nothing <*> _ = Nothing
  (Just f) <*> v = fmap f v

instance Monad Maybe where
  return a = Just a

  (Just x) >>= f = f x
  Nothing >>= _ = Nothing