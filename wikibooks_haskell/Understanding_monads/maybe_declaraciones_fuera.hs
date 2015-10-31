{-Este fichero muestra cómo se pueden poner explícitamente las
  declaraciones de tipos de las funciones cuando estamos instanciando
  un miembro de una clase de tipos-}

import Prelude hiding (Maybe, Nothing, Just)

data Maybe x = Nothing | Just x

{-
nuestras herramientas son
Just :: a -> Maybe a
Nothing :: Maybe a
y el pattern matching
-}

myFmap :: (a -> b) -> Maybe a -> Maybe b
myFmap _ Nothing = Nothing
myFmap f (Just x) = Just (f x) 

instance Functor Maybe where
  fmap = myFmap

-- apply es el nombre que se le da a (<*>)
apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply Nothing  _ = Nothing
apply (Just f) v = fmap f v

instance Applicative Maybe where
  pure = Just

  (<*>) = apply

myBind :: Maybe a -> (a -> Maybe b) -> Maybe b
myBind (Just x) f = f x
myBind Nothing _ = Nothing

instance Monad Maybe where
  return = Just

  (>>=) = myBind