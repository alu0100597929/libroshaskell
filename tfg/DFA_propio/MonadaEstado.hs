module MonadaEstado (Estado(..), evalEstado, execEstado, get, put) where

--significado: el tipo es Estado s a, es decir, al pasarle un s, obtengo (a,s)
newtype Estado s a = Estado { runEstado :: s -> (a, s) }

{-
newtype Estado s a = Estado { runEstado :: s -> (a, s) }
=>
newtype Estado s a = Estado (s -> (a, s))

runEstado :: Estado s a -> s -> (a,s)
runEstado (Estado f) s = f s
-}

evalEstado f s = fst $ runEstado f s
execEstado f s = snd $ runEstado f s

get   = Estado $ \s -> (s,  s)
put s = Estado $ \_ -> ((), s)

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (Estado s) a -> (Estado s) b
-- fmap :: (a -> b) -> (s -> (a,s)) -> (s -> (b,s))

-- inferencia de tipos
-- runEstado :: Estado s a -> s -> (a,s)  --> no hace falta, se puede usar, pero sería redundante
-- h :: s -> (a,s)
-- s :: s

instance Functor (Estado s) where
  -- aquí se hace reconocimiento de patrones y h será una función
  --fmap f (Estado h) = Estado $ \s -> -- como el resultado es un Estado, empezamos poniendo el constructor
  --  let (a, s') = h s
  --  in (f a, s')

  -- aquí NO se hace reconocimiento de patrones y st será una (Estado s a) ó (Estado s) a
  fmap f st = Estado $ \s ->
    let (a,s') = runEstado st s
    in (f a, s')

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (Estado s) (a -> b) -> (Estado s) a -> (Estado s) b
-- "(Estado s) (a -> b)" equivale a "Estado $ \s -> ((a->b), s)" 

instance Applicative (Estado s) where
  pure a = Estado $ \s -> (a,s)
  
  (Estado sf) <*> (Estado sv) = Estado $ \s ->
    let (f,s1)  = sf s
        (a,s2) = sv s1
    in (f a,s2)

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: (Estado s) a -> (a -> (Estado s) b) -> (Estado s) b
instance Monad (Estado s) where
  return = pure

  m >>= f = Estado $ \s ->
    let (a,s') = runEstado m s
    in runEstado (f a) s'