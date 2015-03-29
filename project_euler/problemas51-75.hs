{- problema 56 -}
sumaCifras :: Integer -> Integer
sumaCifras 0 = 0
sumaCifras a = a `mod` 10 + sumaCifras (a `div` 10)

solucion56 = maximum [sumaCifras (a^b) | a <- [1..99], b <- [1..99]]