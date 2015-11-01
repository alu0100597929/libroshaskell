-- MonadExercises.hs

-- El tipo de las funciones es el tipo de dato Reader

newtype Function a b = Function { getFunction :: a -> b }
-- Function :: (a -> b) -> Function a b

instance Functor (Function a) where
  -- fmap :: (b -> c) -> Function a b -> Function a c  
  fmap g (Function h) = Function (g . h)

instance Applicative (Function a) where
  -- pure :: a -> m a
  -- pure :: a -> (Function a) b
  pure x = Function (\_ -> x)

  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: (Function a) (a -> b) -> (Function a) a -> (Function a) b
  (Function g) <*> (Function h) = Function $ \x -> let fun = g x
                                                       value = h x
                                                   in fun value

-- Reader monad
instance Monad (Function a) where
  return = pure

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: (Function a) a -> (a -> (Function a) b) -> (Function a) b
  (Function g) >>= h = Function $ \x -> let result = g x
                                            function = getFunction $ h result
                                        in function x
  -- Definición de Real World Haskell
  -- g >>= h = R $ \r -> runReader (h (runReader g r)) r

-- Implementación de la mónada libre. Se asume que el tipo f es instancia de Functor

data Free f a = Var a
              | Node (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Var a) = Var (f a)
  fmap f (Node xs) = Node $ fmap (fmap f) xs

instance Functor f => Applicative (Free f) where
  -- pure :: a -> m a
  pure = Var

  -- (<*> :: f (a -> b) -> f a -> f b)
  -- (<*> :: (Free f) (a -> b) -> (Free f) a -> (Free f) b)
  (Var f) <*> as = fmap f as
  Node xs <*> as = Node $ fmap (<*> as) xs


instance Functor f => Monad (Free f) where
  return = pure

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: (Free f) a -> (a -> (Free f) b) -> (Free f) b
  (Var x) >>= f = Var (fmap (>>= f) x)
  (Node r) >>= f = f r