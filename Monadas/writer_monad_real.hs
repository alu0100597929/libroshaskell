data Writer m a = Writer m a

-- hagamos las instancias por orden!!!!

-- fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor (Writer m) where
  
  fmap f (Writer m a) = Writer m (f a)
 
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
instance Monoid m => Applicative (Writer m) where
  
  pure a = Writer mempty a

  Writer m f <*> Writer m' a = Writer (mappend m m') (f a)

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
instance Monoid m => Monad (Writer m) where
  
  return a = Writer mempty a

  Writer m a >>= f =
    let Writer m' b = f a -- esto se puede hacer porque el tipo sólo tiene un constructor
      in Writer (mappend m m') b