{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

data Quizas' a = Nada
              | Solo a
              deriving Show

instance Functor Quizas' where
  fmap _ Nada = Nada
  fmap g (Solo x) = Solo (g x)

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

instance Applicative Quizas' where
  pure = Solo

  (Solo f) <*> (Solo x) = Solo (f x)
  _ <*> Nada = Nada
  Nada <*> _ = Nada

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

