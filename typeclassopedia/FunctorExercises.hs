module FunctorExercises where

import Prelude hiding (Either, Left, Right)

{-
1.Implement Functor instances for Either e and ((->) e).
2.Implement Functor instances for ((,) e) and for Pair, defined as

data Pair a = Pair a a

Explain their similarities and differences.

3.Implement a Functor instance for the type ITree, defined as

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

4.Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined). 
5.Is this statement true or false? The composition of two Functors is also a Functor. If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code. 
-}

data Either a b = Left a | Right b


-- 1. Implement Functor instances for Either e and ((->) e).

 
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (Either e) a -> (Either e) b

-- Se puede utilizar un identificador cualificado para referirnos a ella

instance Functor (Either e) where
  fmap _ (Left a)  = Left a
  fmap f (Right b) = Right (f b)

-- instance Functor (FunctorExercises.Either e) where
--   fmap _ (FunctorExercises.Left a) = FunctorExercises.Left a
--   fmap f (FunctorExercises.Right a) = FunctorExercises.Right (f a)

-- otra solución es renombrar el dato Either
data Either' a b = Left' a | Right' b  deriving Show

instance Functor (Either' e) where
  fmap _ (Left' a) = Left' a
  fmap f (Right' a) = Right' (f a)

{-
*FunctorExercises> fmap (+3) (Right' 5)
Right' 8

*FunctorExercises> fmap (+3) (Left' 5)
Left' 5
-}

{-
--este ejercicio

instance Functor ((->) r) where
  fmap f g = (.)

*FunctorExercises> fmap (+ 3) (* 5) 6
33   -- i.e.: 5*6 + 3
-}

newtype Function a b = Function { getFunction :: a -> b }

instance Functor (Function a) where
  fmap g (Function h) = Function (g . h)

incrementar = getFunction $ Function (+1)

{-
You could have let f1 = Function length :: Function [a] Int,
then have fmap show f1 :: Function [a] String, for example.
To run it you would have to use getFunction:
let f1 = Function length;
getFunction (fmap show f1) [1, 2, 3] to get "3" as the result
-}

data Pair a = Pair a a deriving Show

--  la siguiente instancia no aprovecha la "funcionalidad" del segundo argumento

--  instance Functor Pair where
  --  fmap f (Pair a b) = Pair result result
    --  where
      --  result = f a

--  Es mejor poner:

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

{-*FunctorExercises> fmap (+3) (Pair 10 1000)
Pair 13 1003
-}

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node list) = Node (map (fmap g) list)