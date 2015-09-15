-- basado en http://heh.fi/state-monad

import Prelude hiding ((>>=), return)

newtype State s a = S (s -> (a, s))