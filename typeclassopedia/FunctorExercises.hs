module FunctorExercises where

--data Either a b = Left a | Right b

--Implement Functor instances for Either e and ((->) e).

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (Either e) a -> (Either e) b
--instance Functor (Either e) where
  --fmap _ (Left a) = (Left a)
  --fmap g (Right b) = Right (g b)

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r) a -> ((->) r) b
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--instance Functor ((->) r) where
  --fmap f g = \x -> (f . g) x

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair result result 
    where
      result = f a

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((,) e) a -> ((,) e) b
-- fmap :: (a -> b) -> (e,a) -> (e,b)

--instance Functor ((,) e) where
--  fmap f (e,a) = (e,f a)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ITree a -> ITree b
instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node list) = Node (map (fmap g) list)