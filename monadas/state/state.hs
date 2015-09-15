-- basado en http://heh.fi/state-monad

import Prelude hiding ((>>=), (>>), return)

newtype State s a = S (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (S f) s = f s

-- fijo un resultado y permito que le llegue un estado, el cual no cambiará
return :: a -> State s a
return a = S $ \s -> (a, s)

get :: State s s
get = S $ \s -> (s, s)

-- fijo un estado y no dejo recibir otro estado, produce () como resultado
-- s suele ser una función!!!
put :: s -> State s ()
put s = S $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = S $ \s -> ((), f s)

-- para combinar (componer) computaciones con estado
(>>=) :: State s a -> (a -> State s b) -> State s b
m >>= k = S $ \s -> let (a, s') = runState m s
                        nuevaMonada = k a
                    in runState nuevaMonada s'

-- sólo me interesa cambiar la información oculta en la mónada
(>>) :: State s a -> State s b -> State s b
(>>) (S f) (S g) = S $ \s -> let (_, s') = f s
                             in g s'

composicion n = runState (get >>= \s -> put (s+1) >> return "foo") n

composicion' :: (Num a, Show a) => a -> ([Char], a)
composicion' n = runState (get >>= \s -> put (s*s) >> get >>= \s' -> put (s'*s') >> return (show s ++ " a la cuarta = ")) n

encadenada :: Int -> (String, Int)
encadenada n = runState (modify (+5) >> modify (*5) >> return (show n ++ " mas cinco y por cinco = ")) n