fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x cons) = x : streamToList cons

fmapStream :: (a -> b) -> Stream a -> Stream b
fmapStream f (Cons x cons) = (Cons (f x) (fmapStream f cons))

instance Functor (Stream) where
  fmap = fmapStream

sRepeat :: a -> Stream a
sRepeat x = (Cons x (sRepeat x))

--