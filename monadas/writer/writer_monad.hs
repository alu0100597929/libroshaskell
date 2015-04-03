--import Data.Monoid
--import Control.Applicative (Applicative(..))
--import Control.Monad       (liftM, ap)

{-
las reglas de clase Monoid son...

asociatividad: dados a,b,c :: m, mappend a (mappend b c) = mappend (mappend a b) c

elemento neutro: dado a :: m, mappend mempty a = mappend a mempty = a

hay un alias para la función "mappend", que es más común que escribir el operador en sí
(<>) = mappend
usando el operador (<>) las reglas quedan
asociatividad: dados a,b,c :: m, a <> (b <> c) = (a <> b) <> c
elemento neutro: dado a :: m, mempty <> a = a <> mempty = a
-}

instance Monoid Int where
  mappend = (+)
  mempty = 0

{-
*Main> 7 <> 8 :: Int
15
*Main> 7 <> 0 :: Int
0
-}

{-
instance Monoid [a] where
  mappend = (++)
  mempty = []
-}

data Writer m a = Writer m a

class ForWriter t where
  something :: t
  combine :: t -> t -> t

--hagamos las instancias por orden!!!!

--fmap :: Functor f => (a -> b) -> f a -> f b
instance Functor (Writer m) where
  
  fmap f (Writer m a) = Writer m (f a)
 
--pure :: a -> f a
--(<*>) :: f (a -> b) -> f a -> f b
--(<*>) :: Writer m (a -> b) -> Writer m a -> Writer m b
instance ForWriter m => Applicative (Writer m) where
  
  pure a = Writer something a

  Writer m f <*> Writer m' a = Writer (combine m m') (f a)

--return :: a -> m a
--(>>=) :: m a -> (a -> m b) -> m b
instance ForWriter m => Monad (Writer m) where
  
  return a = Writer something a

  Writer m a >>= f =
    let Writer m' b = f a -- esto se puede hacer porque el tipo sólo tiene un constructor
      in Writer (combine m m') b