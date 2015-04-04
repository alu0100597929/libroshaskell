module ExampleState (State(..), evalState, execState, get, put) where

--significado: el tipo es State s a, es decir, al pasarle un s, obtengo (a,s)
newtype State s a = State { runState :: s -> (a, s) }

{-
newtype State s a = State { runState :: s -> (a, s) }
=>
newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a,s)
runState (State f) s = f s
-}

evalState f s = fst $ runState f s
execState f s = snd $ runState f s

get   = State $ \s -> (s,  s)
put s = State $ \_ -> ((), s)

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (State s) a -> (State s) b
-- fmap :: (a -> b) -> (s -> (a,s)) -> (s -> (b,s))

-- inferencia de tipos
-- runState :: State s a -> s -> (a,s)  --> no hace falta, se puede usar, pero sería redundante
-- h :: s -> (a,s)
-- s :: s

instance Functor (State s) where
  -- aquí se hace reconocimiento de patrones y h será una función
  --fmap f (State h) = State $ \s -> -- como el resultado es un State, empezamos poniendo el constructor
  --  let (a, s') = h s
  --  in (f a, s')

  -- aquí NO se hace reconocimiento de patrones y st será una (State s a) ó (State s) a
  fmap f st = State $ \s ->
    let (a,s') = runState st s
    in (f a, s')

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (State s) (a -> b) -> (State s) a -> (State s) b
-- "(State s) (a -> b)" equivale a "State $ \s -> ((a->b), s)" 

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  
  (State g) <*> (State h) = State $ \s ->
    let (a,s') = h s
        (f,_)  = g s
    in (f a,s')

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: (State s) a -> (a -> (State s) b) -> (State s) b
instance Monad (State s) where
  return = pure

  m >>= f = State $ \s ->
    let (a,s') = runState m s
    in runState (f a) s'