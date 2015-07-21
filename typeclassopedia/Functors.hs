import Prelude hiding (Either(..) )

data Either a b = Left a
                | Right b
                deriving Show

instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

{-
Está ya definido en GHC.Base

instance Functor ((->) r) where
  fmap g h = (.)
-}

data Pair a = Pair a a
            deriving Show

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

{-
instance Functor ((,) e) where
  fmap f (x,y) = (x, f y)
-}

{- Esto no se puede hacer instancia de Show porque no se sabe cómo mostrar
   las funciones que contiene 

Node [ Node [even]]

-}

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

prueba = Node [ Node [Leaf even]]

-- fmap :: (a -> b) -> Itree a -> Itree b

instance Functor ITree where
  fmap h (Leaf g) = Leaf $ h . g
  fmap h (Node xs) = Node $ map (fmap h) xs

-- TODO: las demostraciones