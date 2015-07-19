import Data.List

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

sIterate :: (a -> a) -> a -> Stream a
sIterate f semilla = (Cons semilla (sIterate f (f semilla)))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x cons) (Cons y cons') = Cons x (Cons y (sInterleave cons cons'))

sTake :: Int -> Stream a -> [a]
sTake 0 _             = []
sTake n (Cons x cons) = x : sTake (n-1) cons

nats :: Stream Integer
nats = sIterate (1+) 0

potencias2 = streamToList $ sIterate (*2) 1

maximoDivPotencia2 n = case findIndex (\x -> x == (maximum listaDivs)) listaPosiblesDivs of
                         Nothing -> -1
                         Just x -> x
  where
    listaPosiblesDivs = takeWhile (<= n) potencias2
    listaDivs = filter (\x -> n `mod` x == fromIntegral 0) listaPosiblesDivs

--rulerFea :: [a]
{-
El patrón es 1,2,1,3,1,2,1,X con X igual a:
4,5,4,6,4,5,4,7,4,5,4,6,4,5,4,8,4,5,4,6,4,5,4,7,4,5,4,6,4,5,4,9
-}

sacarOctavos [] = []
sacarOctavos xs = let (y:ys) = drop 7 xs
                  in y : sacarOctavos ys