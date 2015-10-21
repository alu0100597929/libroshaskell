-- debemos saber que para instanciar en clases de tipos cosas que ya están
-- por ejemplo en el Prelude, se suelen declarar newtypes o datas

import Prelude hiding (Either, Left, Right)

data Either a b = Left a
                | Right b
                deriving Show

instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right $ f b

-- un tipo que encapsula una función (a -> b)
newtype Function a b = Function { getFunction :: a -> b }

instance Functor (Function a) where
  fmap f (Function h) = Function $ \x -> f (h x) -- Function (f . h)

-- ghci> let composicion = fmap (*3) (Function (+2))
-- ghci> getFunction composicion 9
-- ghci> 33

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- Ahora hacemos lo mismo para el tipo (,) e
-- las siguientes dos líneas son equivalentes al newtype de abajo
-- newtype Pair' b a = Pair' (a, b)

-- getPair (Pair' (x, y)) = (x, y)

newtype Pair' a = MkPair { getPair :: (a, a) }

instance Functor Pair' where
  fmap f (MkPair (x, y)) = MkPair (f x, f y)

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

-- fmap :: (a -> b) -> ITree a -> ITree b
instance Functor ITree where
  fmap g (Leaf h)  = Leaf $ \x -> g (h x)
  fmap g (Node xs) = Node $ map (fmap g) xs

{-
Node [Node [Leaf (+5)],
      Leaf (*7),
      Node [Leaf (*3)],
-}

-- lo único que sabemos es que f es instancia de Functor
data ITree' f a = Leaf' (Int -> a) | Node' (f (ITree' f a))

instance Functor f => Functor (ITree' f) where
  fmap g (Leaf' h)     = Leaf' $ \x -> g (h x)
  fmap g (Node' arbol) = Node' $ fmap (fmap g) arbol

-- Para entender esta definición de tipo debemos saber:
-- en las declaraciones de tipos SOLO puede haber tipos

-- f = []
-- f = Maybe
-- f = Identity
-- f = cualquier otro tipo instancia de Functor

data Phantom f a = Phantom (f Int)