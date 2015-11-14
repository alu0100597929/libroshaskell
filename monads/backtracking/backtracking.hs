-- http://www.randomhacks.net/2007/03/12/monads-in-15-minutes/

import Prelude hiding ((>>=), return)

type Choice a = [a]

choose :: [a] -> Choice a
choose xs = xs

pair456 :: Int -> Choice (Int,Int)
pair456 x = choose [(x,4), (x,5), (x,6)]

-- > map pair456 (choose [1,2,3])
--[[(1,4),(1,5),(1,6)],
-- [(2,4),(2,5),(2,6)],
-- [(3,4),(3,5),(3,6)]]

join :: Choice (Choice a) -> Choice a
join choices = concat choices

-- > join (map pair456 (choose [1,2,3]))
--[(1,4),(1,5),(1,6),
-- (2,4),(2,5),(2,6),
-- (3,4),(3,5),(3,6)]

(>>=) :: Choice a -> (a -> Choice b) -> Choice b
choices >>= f = join (map f choices)

-- > choose [1,2,3] >>= pair456
--[(1,4),(1,5),(1,6),
-- (2,4),(2,5),(2,6),
-- (3,4),(3,5),(3,6)]

return :: a -> Choice a
return x = choose [x]

makePairs :: Choice (Int,Int)
makePairs = 
  choose [1,2,3] >>= (\x ->
  choose [4,5,6] >>= (\y ->
  return (x,y)))

-- > makePairs
--[(1,4),(1,5),(1,6),
-- (2,4),(2,5),(2,6),
-- (3,4),(3,5),(3,6)]

makePairs' :: Choice (Int,Int)
makePairs' = do
  x <- choose [1,2,3]
  y <- choose [4,5,6]
  return (x,y)

-- > makePairs'
--[(1,4),(1,5),(1,6),
-- (2,4),(2,5),(2,6),
-- (3,4),(3,5),(3,6)]

-- Define a "zero" for our monad.  This
-- represents failure.
mzero :: Choice a
mzero = choose []

-- Either fail, or return something
-- useless and continue the computation.
guard :: Bool -> Choice ()
guard True  = return ()
guard False = mzero

solveConstraint = do
  x <- choose [1,2,3]
  y <- choose [4,5,6]
  guard (x*y == 8)
  return (x,y)