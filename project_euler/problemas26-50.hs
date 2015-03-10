import Data.List
import Data.Char
import Data.Maybe
--import Data.Numbers -- ya no existe
import Data.Numbers.Primes --  chetos xD
import Data.List.Split (splitOn, splitPlaces) --  parte las listas donde encuentre ocurrenias de lo que le pasemos
import qualified Data.Set as Set

{-problema 25-}

fibs = 1 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]

num_cifras :: Integer -> Integer
num_cifras 0 = 0
num_cifras n = 1 + num_cifras (n `div` 10)

-- recuerda, los índices de las listas también empiezan en 0
primerFib1000 = findIndex (\x -> (num_cifras x) == 1000) fibs

{-problema 27-}

-- Cuidado!! la función esPrimo se tragaba los negativos, y los primos sólo son positivos...

raizNumEntero :: Int -> Int
raizNumEntero = floor . sqrt . fromIntegral

esPrimo :: Int -> Bool
esPrimo k = if (k >= 2)
              then null [x | x <- [2..raizNumEntero k], k `mod`x  == 0]
            else False

formulaPrimos :: Int -> Int -> Int -> Int
formulaPrimos n a b = n^2 + a*n + b

numPrimosFormula a b = length (takeWhile (\x -> esPrimo (formulaPrimos x a b)) [0..])

maximosPrimos = maximum [(numPrimosFormula a b) | a <- [-999..999], b <- [-999..999]]

numPosibilidades = sum [1 | a <- [-9..9], b <- [-9..9]]

indices = [a*b | a <- [-999..999], b <- [-999..999], (numPrimosFormula a b) == max]
          where max = maximosPrimos

{-problema 32-}

enteroALista :: Int -> [Int]
enteroALista 0 = []
enteroALista n = enteroALista (n `div` 10) ++ [n `mod` 10]

cuantosContiene :: [Int] -> Int
cuantosContiene xs = length $ filter (\x -> x `elem` xs) [1..length xs]

esPandigital :: [Int] -> Bool
esPandigital xs = length xs == 9 && cuantosContiene xs == 9

triplePandigital :: (Int, Int, Int) -> Bool
triplePandigital (a,b,c) = esPandigital $ enteroALista a ++ enteroALista b ++ enteroALista c

-- poner el tipo de triple resultante, si no, trabaja con Integer
productos :: [(Int,Int,Int)]
productos = [(a,b,a*b) | a <- [1..3000], b <- [1..3000]]

prodPandigitales = filter (\x -> triplePandigital x) productos

tercero (a,b,c) = c

quitarRepetidos' :: [(Int,Int,Int)] -> [Int] -> [Int]
quitarRepetidos'     [] aux = aux
quitarRepetidos' (x:xs) aux = if (prod) `elem` aux 
                then quitarRepetidos' xs aux
                else quitarRepetidos' xs (prod:aux)
                where
                  prod = (tercero x)

quitarRepetidos xs = quitarRepetidos' xs []

sumaProdPandigitales = sum $ quitarRepetidos $ prodPandigitales

{-problema 33-}

{-Laconic solution (requires Data.Function)

foo = (/) `on` fromIntegral-}

valorFraccion :: (Int, Int) -> Float
valorFraccion (a,b) = (fromIntegral a) / (fromIntegral b)

fraccionesEquivalentes :: (Int, Int) -> (Int, Int) -> Bool
fraccionesEquivalentes (a,b) (c,d) = valorFraccion (a,b) == valorFraccion (c,d)

borrar :: Int -> [Int] -> [Int]
borrar x xs = 0 : (delete x xs)

seSimplifico :: (Int, Int) -> Bool
seSimplifico (a,b) = a /= c
  where
    (c,d) = simplificarMal (a,b) 

-- sólo trabaja con fracciones de dos cifras en denominador y numerador, si da números de una cifra
-- entonces pudo simplificar
simplificarMal :: (Int, Int) -> (Int, Int)
simplificarMal (a,b) = if (a `mod` 10 == 0 || b `mod` 10 == 0) 
                         then (a,b)
                         else if primera_cifra `elem` lista_den
                           then (listaAInt $ borrar primera_cifra lista_num, listaAInt $ borrar primera_cifra lista_den)
                           else if segunda_cifra `elem` lista_den
                             then (listaAInt $ borrar segunda_cifra lista_num, listaAInt $ borrar segunda_cifra lista_den)
                             else (a,b)
  where
    primera_cifra = (lista_num !! 0)
    segunda_cifra = (lista_num !! 1)
    lista_num = intALista a
    lista_den = intALista b

-- sabemos que son fracciones impropias, por tanto el numerador será menor estricto que el denominador
fracs :: [(Int, Int)]
fracs = [(a,b) | a <- [11..99], b <- [a..99], a /= b, seSimplifico (a,b), fraccionesEquivalentes (a,b) (simplificarMal (a,b))]

solucion33 = p_denominadores `div` mcd
  where
    mcd = euclides p_numeradores p_denominadores
    p_numeradores = product . map fst $ fracs
    p_denominadores = product . map snd $ fracs

{-problema 34-}

integerALista :: Integer -> [Integer]
integerALista n
  | n <= 0 = []
  | otherwise = integerALista (n`div` 10) ++ [n `mod` 10]

factorial :: Integer -> Integer
factorial n
  | n < 0 = error "la función factorial no se aplica a números negativos"
  | n == 0 = 1
  | otherwise = product [1..n]

esCandidato n = n == sum (map (factorial) $ integerALista n)

-- 9! = 362880
solucion34 = sum (filter (esCandidato) [3..362880])

{-problema 36-}

esPalindromo' :: (Eq a) => [a] -> Bool
esPalindromo' xs
  | null xs = True
  | head xs == last xs = esPalindromo' (if not (null (init xs))
                                          then tail (init xs)
                                          else [])
  | otherwise = False

esPalindromo'' :: (Eq a) => [a] -> Bool
esPalindromo'' xs = xs == reverse xs

esPalindromo :: Int -> Bool
esPalindromo n = esPalindromo' (enteroALista n)

decimalABinario' :: Int -> [Int]
decimalABinario' 0 = [0]
decimalABinario' 1 = [1]
decimalABinario' n = (n `mod` 2) : decimalABinario' (n `div` 2)

decimalABinario n = reverse $ decimalABinario' n

esPalindromoBin n = esPalindromo' (decimalABinario n)

sumaPalindromosIntBin = sum $ filter (\x -> esPalindromo x && esPalindromoBin x) [1..1000000]

{-problema 37-}

primoIzquierda' :: [Int] -> Bool
primoIzquierda' [] = True
primoIzquierda' xs = ((esPrimo izquierda) || izquierda == 0) && primoIzquierda' (init xs)
  where izquierda = listaAInt $ init xs

primoDerecha' :: [Int] -> Bool
primoDerecha' [] = True
primoDerecha' xs = ((esPrimo derecha) || derecha == 0) && primoDerecha' (tail xs)
  where derecha = listaAInt $ tail xs

primoTruncable n = let entero = intALista n in
                     if n < 10
                       then False
                       else primoIzquierda' entero && primoDerecha' entero

solucion37 = sum . take 11 $ filter (primoTruncable) primes

--main = print solucion37

{-problema 39-}

{-As a starter, based on the relation
   a^2+b^2 = c^2  (1)
If both a and b are even, c will also be even and P (the perimeter) will be even.
If both a and b are odd, c will be even and P will be even.
If one is even and the other is odd, c will be odd and P will again be even.
Therefore, only even values of P need to be checked.

Based on the other equation
  a+b+c = P      (2)
and using the value of c=P-a-b to replace it in equation (1) above, the following is obtained:
  a^2+b^2 = (P-a-b)^2 = P^2+a^2+b^2-2*P*a-2*P*b+2*a*b (3)

Simplifying and transposing gives
   b = P*(P-2*a)/2(P-a)  (4)

For all even values of P, try all values of a with this equation until it exceeds or equals the
computed value of b, counting the number of times that the division yields a whole number.-}


{-Versión preliminar mía:
solsSumaPerimetro p = [p | c <- [1..p], b <- [1..c-1], a <- [p-b-c], a < b, a^2 + b^2 == c^2]

-- cuidado, para listas es maximum y no max a secas!!
perimetroMaxTriangulos = (findIndex (\x -> (length x) == longitud_maxima) triangulos)
               where 
                   longitud_maxima = maximum $ map (length) triangulos
                   triangulos = map (\x -> solsSumaPerimetro x) [2,4..1000]

-- en Haskell los índices también empiezan en 0, de ahí el +1, además, sólo comprobamos los pares,
-- por tanto debemos multiplicar ese índice por 2
solucion = (fromJust perimetroMaxTriangulos + 1) * 2-}

-- versión pro:
-- comprueba si un flotante es entero positivo
isInt :: (RealFrac a) => a -> Bool
isInt x = x == entero && entero > 0
  where entero = fromInteger (round x)

-- se usa Fractional para divisiones enteras de las cuales queramos un resultado real
valorB :: Fractional a => a -> a -> a
valorB p a = p * (p - 2 * a) / (2 * (p - a)) -- devuelve el valor de b, si a es entero, también a, por
                                             -- tanto no hay que calcular c con c = P - a - b

-- valores de A para los cuales B es entero
valoresA p = filter (\a -> isInt (valorB p a)) [1..p/2]

parAB :: Fractional t => t -> t -> (t, t)
parAB p a = (a, valorB p a)

solucionesP p = length $ takeWhile (\(a,b) -> a < b) $ map (\a -> parAB p a) (valoresA p)

perimetroMaxSols = case (findIndex (\a -> a == max_sols) sols_p) of Just x -> x + 1
                                                                    Nothing -> -1
          where
            max_sols = maximum sols_p
            sols_p = map (solucionesP) [1..1000]

--  *************************-- 
--  *************************-- 
-- solucion39 = print perimetroMaxSols
--  *************************-- 
--  *************************-- 

{-problema 42-}

-- comprueba si un elemento es parte de una lista infinita (debe estar ordenada de manera no decreciente)
elemInfinita :: (Ord a) => a -> [a] -> Bool
elemInfinita x (y:ys)
  | x > y     = elemInfinita x ys
  | x == y    = True
  | otherwise = False

triangular :: Int -> Int
triangular n = (n^2 + n) `div` 2

triangulares :: [Int]
triangulares = map triangular [1..]

alfabetoIngles :: Char -> Int
alfabetoIngles c = case findIndex (\x -> x == c) ['A'..'Z'] of Just x  -> succ x
                                                               Nothing -> 0

valorString :: String -> Int
valorString = sum . map alfabetoIngles

palabraTriangular :: String -> Bool
palabraTriangular str = elemInfinita (valorString str) triangulares

solucion42 = do
               contents  <- fmap (length . filter palabraTriangular . splitOn ",") $ readFile "p042_words.txt"
               print contents

{-problema 43-}

intALista :: Int -> [Int]
intALista n
  | n <= 0 = []
  | otherwise = intALista (n`div` 10) ++ [n `mod` 10]

listaAInt :: [Int] -> Int
listaAInt = foldl (\z x -> 10*z + x) 0

permutaciones :: Int -> [[Int]]
permutaciones = permutations . intALista

grupos :: [Int] -> [Int]
grupos xs = if length grupo3 == 3 then listaAInt grupo3 : grupos cola
                                  else []
  where
    grupo3 = take 3 cola
    cola   = tail xs

grupoAInt :: [Int] -> Int
grupoAInt xs = elQueFalta * 1000000000 + faltaUno 
  where
    elQueFalta = cualFalta listaIncompleta
    listaIncompleta = intALista faltaUno
    faltaUno = (xs !! 0)*1000000 + (xs !! 3)*1000 + xs !! 6

cualFalta :: [Int] -> Int
cualFalta xs = case find (\x -> x `notElem` xs) [0..9] of Just x -> x
                                                          Nothing -> 0

-- hacemos esto para currificar respecto al primer parámetro
divisible :: Int -> Int -> Bool
divisible m n = n `mod` m == 0

listaDivisores :: [Int]
listaDivisores = [2, 3, 5, 7, 11, 13, 17]

condicion :: [Int] -> Bool
condicion = and . zipWith (divisible) listaDivisores

solucion43 = sum . map grupoAInt . filter condicion . map grupos $ permutaciones 9876543210

-- main = print solucion43

{-Problema 43-}

nPentagonales :: Int -> [Int]
nPentagonales n = map (\x -> (3 * x^2 - x) `div` 2) [1..n]

{-Problema 46-}

escribible :: Int -> Bool
escribible n
  | esPrimo n = False
  | otherwise = not . null $ [x + y | x <- primos_menores, y <- dobles_cuadrados, x + y == n]
  where
    primos_menores = takeWhile (< n) primes
    dobles_cuadrados = takeWhile (< n) $ map (\x -> 2*x*x) [1..]

menor_no_escribible :: Int
menor_no_escribible = case indice of Just x -> imparesNoPrimos !! x
                                     Nothing -> error "no hay solucion" 
  where
    indice = findIndex ((==) False) (escribibles)
    escribibles = map escribible imparesNoPrimos
    imparesNoPrimos = [x | x <- [9,11..], not $ esPrimo x]

solucion46 :: Int
solucion46 = menor_no_escribible

{-problema 47-}

{-factoresPrimos :: Int -> [Int]
factoresPrimos n
  | n == 1 = [] --el 1 es factor, pero no es primo
  | otherwise = case factor of Just x -> x : factoresPrimos (n `div` x)
                               Nothing -> error "puta bida tt"
  where
    factor = find (\x -> n `mod` x == 0) primes

reducirFactores :: [Int] -> [Int]
reducirFactores = map (product) . group

listasDisjuntas :: (Eq a) => [[a]] -> Bool
listasDisjuntas xs = all (== (length xs)) $ map length xs

gruposDeN :: Int -> [Int] -> [[Int]]
gruposDeN n xs = zipWith (\x y -> take (y - x) $ drop x xs) [0..length xs - n] [n..length xs]

solucion47 = case findIndex (listasDisjuntas) $ factores of Just x -> succ x
                                                            Nothing -> error "no hay"
  where
    factores = map (map (reducirFactores . factoresPrimos)) grupos
    grupos = gruposDeN 3 [1..1000000]
-}

factors :: [[Int]]
factors = map primeFactors [1..]

main :: IO ()
main = do
  let n = 4 -- number of factors to go for
      nums = zip [2..] $ map (take n) $ tails $ map (nub.primeFactors) [2..]
      -- nums is now a list of (n,[fac n, fac n+1, fac n+2, fac n+3])
      hasCount = all ((==n).length) . snd
  let nums' = filter hasCount nums
      -- nums' only contains sequences with the right number of factors
  print $ head $ nums'

{-Problema 48-}

solucion48 = sum [x^x | x <- [1..1000]] `mod` 10000000000

{-problema 49-}

esPermutacion :: Int -> Int -> Bool
esPermutacion x y = intALista x `elem` permutations (intALista y) 

condicion49 :: Int -> Int -> Maybe (Int, Int, Int)
condicion49 x y = if (esPrimo (x + y) && esPrimo (x + 2*y) &&
                      x `esPermutacion` (x + y) && x `esPermutacion` (x + 2*y))  
                    then Just (x, x + y, x + 2*y)
                    else Nothing

--por ecuación sabemos que x (el número a sumar) va de 1 a (9973 - x)
intentar :: Int -> [(Int, Int, Int)]
intentar x = mapMaybe (condicion49 x) [1..(9973 - x) `div` 2]

solucion49 = let listaPrimos = filter (> 1000) $ takeWhile (< 10000) primes in 
               (filter (not . null) $ map (intentar) listaPrimos) !! 1

--main = print solucion49

{-problema 204-}

-- algoritmo de Euclides, para calcular el máximo común divisor
euclides :: Int -> Int -> Int
euclides a b
  | b == 0 = a
  | otherwise = euclides b (a `mod` b)

cuadradoModulo :: Int -> Int -> Int
cuadradoModulo a b = a^2 `mod` b

todosRes :: Int -> [Int]
todosRes b = map (\x -> cuadradoModulo x b) [1..115]

-- quadratic sieve or dead xD

cuadrados :: Int -> [Int]
cuadrados n = map (^2) [1..n]

-- queremos factorizar 1649

-- num_hamming = length $ filter (\a -> (mayorFactorPrimo a) <= 97) [2..1000000000]

{-problema 50-}

-- TODO

sumaPrimos :: Int -> [Int]
-- sumaPrimos n = last . filter (esPrimo) . takeWhile (< n) $ scanl (+) 0 primes
sumaPrimos n = takeWhile (< n) $ scanl (+) 0 primes

-- sumaPrimaPrimos :: [Int]
-- sumaPrimaPrimos = filter (\x -> esPrimo $ sumaPrimos x) $ takeWhile (< 1000) primes