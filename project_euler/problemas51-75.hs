import Data.List

{- problema 52 -}

intALista :: Int -> [Int]
intALista 0 = []
intALista n = intALista (n `div` 10) ++ [n `mod` 10]

esPermutacion :: Int -> Int -> Bool
esPermutacion a b = (sort $ intALista a) == (sort $ intALista b)

condiciones :: Int -> Bool
condiciones x = and $ map (\y -> esPermutacion x $ x*y) [2..6]

solucion52 = find (condiciones) [1..]

{- problema 56 -}
sumaCifras :: Integer -> Integer
sumaCifras 0 = 0
sumaCifras a = a `mod` 10 + sumaCifras (a `div` 10)

solucion56 = maximum [sumaCifras (a^b) | a <- [1..99], b <- [1..99]]