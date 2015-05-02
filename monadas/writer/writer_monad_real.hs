import Control.Monad

data Writer m a = Writer m a
                deriving Show

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
-- (>>=) :: Writer m a -> (a -> Writer m b) -> Writer m b
instance Monoid m => Monad (Writer m) where
  
  return = pure
  --return a = Writer mempty a

  Writer m a >>= f =
    let Writer m' b = f a -- esto se puede hacer porque el tipo sólo tiene un constructor
      in Writer (mappend m m') b

-- ejemplo de uso de foldM

sumaPasoAPaso :: Double -> Double -> Writer String Double
sumaPasoAPaso x1 x2 = Writer (show x1 ++ " + " ++ show x2 ++ " = " ++ show suma ++ ". ") suma
  where
    suma = x1 + x2

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
-- foldM :: (b -> a -> Writer m b) -> b -> [a] -> Writer m b
ops :: [Double] -> Writer String Double
ops = foldM sumaPasoAPaso 0