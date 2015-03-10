estael3 [] = False
estael3 (x:xs) = (x == 3) || estael3 xs

-- podría haberse escrito como iterate (succ)
desde :: Integer -> [Integer]
desde x = x : desde (succ x)

suma :: Integer -> [Integer] -> Integer
suma n (x:xs) = if n == 0 then 0 else x + suma (n-1) xs

selec :: Integer -> [Integer] -> Integer
selec n (x:xs) = if n == 1 then x else selec (n-1) xs

incr :: [Int] -> [Int]
incr (n:ns) = (n + 1) : incr ns -- map (1+)

aniadeCero :: [Int] -> [Int]
aniadeCero = (0:)

pos = 0 : incr pos

{-
selec 2 pos => selec necesita separar en cabeza y cola
selec 2 (0 : incr pos) => 2) selec
selec 1 (incr pos) => def. de pos
selec 1 (incr (0 : incr pos)) => def. de incr para cabeza y cola
selec 1 (1: incr (incr pos)) => 1) selec
1
-}

-- Criba de Eratóstenes
primes :: [Integer]
primes = primes' [2..]

primes' :: [Integer] -> [Integer]
primes' (x:xs) = x : primes' (filtrada)
  where
    filtrada = [y | y <- xs, y `mod` x /= 0]

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

facts = 1 : zipWith (*) [1..] facts

--ejercicio 8.9
facts' = 1 : zipWith (*) (iterate (+1) 1) facts'

pascal :: [[Integer]]
pascal = [1] : map f pascal
  where f cs = zipWith (+) (0:cs) (cs ++ [0])