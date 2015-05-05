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

{- problema 59 -}

fillWithZeroes xs = relleno ++ xs
  where
    relleno = take (8 - largo) $ repeat 0
    largo = length xs

dec2Bin' 0 = [0]
dec2Bin' 1 = [1]
dec2Bin' n = n `mod` 2 : dec2Bin' (n `div` 2)

dec2Bin n = (fillWithZeroes . reverse) $ dec2Bin' n

bin2Int xs = sum $ map (\(e,v) -> (2^e)*v) $ zip [length xs - 1, length xs - 2..0] xs

myXor' = zipWith (\x y -> if x == 1 || y == 1 && not (x == 1 && y == 1)
                            then 1
                            else 0)

--recibe dos enteros y les aplica la XOR binaria, da otro entero
myXor m n = bin2Int $ myXor' (dec2Bin m) (dec2Bin n)

-- sabemos que la clave está formada por tres letras minúsculas
asciiBin = [97..122]

leerFichero :: String -> IO ()
leerFichero filename = do
    contenidos <- readFile filename
    putStrLn contenidos