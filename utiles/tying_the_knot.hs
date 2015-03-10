fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

facts = 1 : zipWith (*) [1..] facts

-- Criba de Eratóstenes
primes :: [Integer]
primes = primes' [2..]

primes' :: [Integer] -> [Integer]
primes' (x:xs) = x : primes' (filtrada)
  where
    filtrada = [y | y <- xs, y `mod` x /= 0]