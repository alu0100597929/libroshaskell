-- basado en http://heh.fi/state-monad

import Prelude hiding ((>>=), return)

newtype State s a = S (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (S f) s = f s

return :: a -> State s a
return a = S $ \s -> (a,  s)

get :: State s s
get = S $ \s -> (s, s)

put :: s -> State s ()
put s = S $ \_ -> ((), s)

(>>=) :: State s a -> (a -> State s b) -> State s b
m >>= k = S $ \s ->
  let (a, s') = runState m s
  in  runState (k a) s'