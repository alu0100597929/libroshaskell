import Data.Natural
import Data.List((\\))
import Data.Array

minfree :: [Natural] -> Natural
minfree xs = head ([0..] \\ xs)

-- *Main> minfree [8,23,9,0,12,11,1,1,13,7,41,4,14,21,5,17,3,19,2,6]
-- 15

{-
The expression us \\ vs denotes the list of those elements of us that remain
after removing any elements in vs:
(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

*Main> minfree $ [14,13..7] ++ [5,4..0]
6
*Main> minfree $ (\\) [14,13..0] [9]
9
-}

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

{-
The functions indices, elems, and assocs, when applied to an array, return lists of the indices,
elements, or associations, respectively, in index order.
-}

{-
Nothing >>= f = Nothing
(Just x) >>= f = Just (f x)

m >> k      = m >>= \_ -> k

case m of Nothing -> Nothing
          Just x  -> k

"a <- m ; e"
en "m >>= \a -> e"
y "m ; n" en "m >> n"

do c1 <- gooddiv a1 b1 ; c2 <- gooddiv a2 b2 ; return (c1 + c2)

gooddiv a1 b1 >>= (\c1 -> gooddiv a2 b2 >>= (\c2 -> return (c1 + c2)))
-}