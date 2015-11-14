module StateDaniel (State(..), f, get, compose, increaseState, increaseUntil5) where
-- import qualified Prelude

newtype State s a = State { runState :: s -> (a,s) }

{-
newtype State s a = State { runState :: s -> (a, s) }
=>
newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a,s)
runState (State f) s = f s
-}

f x = State $ \s -> (x,s)

get = State $ \s -> (s,s)

-- esto es casi composición, pero tiene un problema y es que pasamos
-- del valor a
compose :: State s a -> State s b -> State s a
compose (State f) (State g) = State $ \s -> f (snd (g s))

-- ejemplo real de la mónada funcionando
increaseState :: State Int ()
increaseState = State $ \s -> ((), s + 1)

increaseUntil5 :: State Int ()
increaseUntil5 = get >>= \s -> if s >= 5
                                 then return ()
                                 else increaseState >> increaseUntil5

-- creo que en la clase funtor no se debe cambiar el estado, sólo el dato del container
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (x,_) = g s in
                                   (f x, s)

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)

  -- la dificultad de esto es la lambda. En los cuerpos de las funciones solo podemos usar
  -- constructores de valor, por tanto, para crear una State necesitamos crear una función
  -- que, recibiendo como argumento s devuelva un par (a,s), y esta lambda hace justo eso

  -- h :: s -> (b,s)

  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State g = State $ \s -> let (x, _)  = g s
                                          (h, s') = f s in
                                          (h x, s')
         
instance Monad (State s) where
  -- return :: a -> State s a
  return = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- f :: (s -> (a,s))
  -- g :: (a -> State s b)
  (State f) >>= g = State $ \s -> let (a,s')  = f s
                                      State h = g a
                                  in h s'

  --m >>= f = State $ \s ->
  --  let (a,s') = runState m s
  --  in runState (f a) s'