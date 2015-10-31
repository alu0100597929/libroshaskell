import Prelude hiding (Maybe, Just, Nothing)

{-
class Functor f => Applicative f where
  pure  :: a -> f a  -- encapsula un tipo sin contexto en un contexto sin efectos
  (<*>) :: f (a -> b) -> f a -> f b  -- aplica una función encapsulada en un contexto a un dato en contexto
-}

data Maybe a = Just a
             | Nothing
             deriving Show

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Applicative Maybe where
--pure  :: a -> f a  -- encapsula un tipo sin contexto en un contexto sin efectos
  pure = Just

--(<*>) :: f (a -> b) -> f a -> f b  -- aplica una función encapsulada en un contexto a un dato en contexto
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just g) <*> (Just x) = Just $ g x

-- Tipo ZipList

-- getZipList :: ZipList a -> [a]
newtype ZipList a = ZipList { getZipList :: [a] }
                  deriving Show

instance Functor ZipList where
  fmap _ (ZipList []) = ZipList []
  fmap f (ZipList xs) = ZipList $ map f xs

instance Applicative ZipList where
  pure x = ZipList [x]

  -- se hace zipWith de las listas con aplicación de funciones
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

{-
instance Applicative [] where
  pure x    = [x]
  gs <*> xs = [ g x | g <- gs, x <- xs ]

-- Usage examples

*Main> [(*2),(*3),(*4)] <*> [1,2,3]
[2,4,6,3,6,9,4,8,12]

pure (+) <*> [2,3,4] <*> pure 4  equivale a:
(+) <$> [2,3,4] <*> pure 4

[6,7,8]
-}

-- faltan los funtores monoidales y la parte más teórica y de demostraciones