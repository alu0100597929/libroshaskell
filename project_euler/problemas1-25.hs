import Data.List
import Data.List.Split -- cabal install split
import Data.Char
import Data.Numbers.Primes
import System.Environment --getArgs
--import Math.NumberTheory.Primes.Factorisation aún no funciona arithmoi
import Control.Monad
import qualified Data.Matrix as M --para el problema 11
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Array.IO

{-Problema 1-}

divisible :: Int -> Int -> Bool
divisible n m = n `mod` m == 0

sumaMultiplos :: Int
sumaMultiplos = sum (filter (\x -> divisible x 3 || divisible x 5) [3..999])

{-Problema 2-}

fib :: Int -> Int
fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)

sumaFib :: Int
sumaFib = sum (map (fib) (filter (\x -> divisible (fib x) 2) (takeWhile (\x -> fib x < 4000000) [1..])))

{-Problema 3-}

raizNumEntero :: Int -> Int
raizNumEntero = floor . sqrt . fromIntegral

esPrimo :: Int -> Bool
esPrimo k = if (k >= 2) then
              null [x | x <- [2..raizNumEntero k], k `mod`x  == 0]
            else
              False

listaPrimos :: Int -> [Int]
listaPrimos n = take n (filter (esPrimo) [2..])

mayorFactorPrimo :: Int -> Int
mayorFactorPrimo n = mayorFactorPrimo' n (listaPrimos n)

mayorFactorPrimo' :: Int -> [Int] -> Int
mayorFactorPrimo' n lista
  | n == 1 = error "El uno no tiene factores primos"
  | cociente == 1 = n
  | (divisible n (head lista)) = mayorFactorPrimo' (cociente) (lista)
  | otherwise = mayorFactorPrimo' n (tail lista)
  where
    cociente = n `div` (head lista)

{-Problema 4-}

--no usada, pero puede servir para más adelante
indexarInt :: Int -> [(Int, Int)]
indexarInt n = zip (intALista n) [0..(num_cifras n) - 1]

--no usada, divide n entre m, o veces, y devuelve el resultado
ndivs n m 0 = n
ndivs n m o = ndivs (n `div` m) m (o-1)

num_cifras :: Int -> Int
num_cifras 0 = 0
num_cifras n = 1 + num_cifras (n `div` 10)

intALista :: Int -> [Int]
intALista n
  | n <= 0 = []
  | otherwise = intALista (n`div` 10) ++ [n `mod` 10]

esPalindromo' :: [Int] -> Bool
esPalindromo' xs
  | null xs = True
  | primera_cifra == ultima_cifra = esPalindromo' (if not (null (init xs)) then tail (init xs) else [])
  | otherwise = False
  where
    primera_cifra = head xs
    ultima_cifra = last xs
    ncifras = length xs

esPalindromo :: Int -> Bool
esPalindromo n = esPalindromo' (intALista n)

--le paso dos números, n y m, y me devuelve el mayor palíndromo resultado de la multiplicación
--de números en ese rango
mayorPalindromo :: Int -> Int -> Int
mayorPalindromo n m = maximum (filter (esPalindromo) (combinaciones n m))

combinaciones :: Int -> Int -> [Int]
combinaciones n m = concat [[x*y | x <- [n..m]] | y <- [n..m]]

{-problema 5-}

divisibleDesdeHasta :: Int -> Int -> Int -> Bool
divisibleDesdeHasta n i j = all (\x -> n `mod` x == 0) [i..j]
--divisibleDesdeHasta n i j = length (takeWhile (\x -> n `mod` x == 0) [i..j]) == j - i + 1

--recuerda, find encuentra el primer elemento que cumple un predicado
menorNum = find (\x -> divisibleDesdeHasta x 1 20) [2521..]

{-problema 6-}

diferencia = (sum [1..100])^2 - sum [x*x | x <- [1..100]]

{-problema 7-}

primo10001 = last (listaPrimos 10001)

{-problema 8-}

stringAListaInt :: [Char] -> [Int]
stringAListaInt = map digitToInt

listaProductos13 :: [Char] -> [Int]
listaProductos13 [] = []
listaProductos13 s = product (take 13 (stringAListaInt s)) : listaProductos13 (tail s)

maximoProducto13 s = maximum (listaProductos13 s)

--es lo mismo String que [Char]

{-
main = do s <- readFile "/home/freinn/librosHaskell/project_euler/numero1000cifras.txt"
    print (stringAListaInt s)
    putStr "\n"
    print (length s)
    putStr "\n"
    print (maximoProducto13 s)
-}

{-problema 9-}

--all empezar b en el a actual más uno nos aseguramos de que será mayor, lo mismo con c respecto a b
--esto mejora bastante el rendimiento
triplete = [(a,b,c) | a <- [1..500], b <- [a+1..500], c <- [b+1..500], a^2 + b^2 == c^2, a+b+c == 1000]

{-problema 10-}

--usamos la función primes (cabal install primes y luego el import) que genera primos de manera muy rápida

sumaPrimos = sum (takeWhile (\x -> x < 2000000)(primes))

{-problema 11-}

stringAListaInt' :: [[String]] -> [[Int]]
stringAListaInt' = map (map read)

--importante, uso de liftM!!!
leerMatriz :: IO [[String]]
leerMatriz = fmap (map (splitOn " ")) $ liftM lines $ readFile "p011matrix.txt"

filaALista :: Int -> M.Matrix a -> [a]
filaALista i mat = V.toList fila
  where
    fila = M.getRow i mat

prodHorizontales i mat = map (product . (\j -> map (\k -> mat M.! (i,j+k)) [0..3])) [1..M.ncols mat-3]
  where
    listaFila = filaALista i mat 

prodVerticales mat = map (\i -> show (M.getCol i mat)) [1..M.nrows mat]

solucion11 = do
               matriz <- fmap (M.fromLists . stringAListaInt') leerMatriz
               print (matriz M.! (1,1) + matriz M.! (20,20))
               print (prodHorizontales 1 matriz)
               print (filaALista 1 matriz)

{-problema 12-}

--devuelve el número de divisores de un número
divisores :: Int -> Int
divisores n = length (filter (\x -> divisible n x) [1..ceiling (sqrt (fromIntegral  n))])

triangular :: Int -> Int
triangular n = n * (n-1) `div` 2

listaTriangulares :: [Int]
listaTriangulares = map (triangular) [2..]

--scanl is similar to foldl, but returns a list of successive reduced values from the left:
--scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--Note that last (scanl f z xs) == foldl f z xs.
listaTriangulares' :: [Int]
listaTriangulares' = scanl (+) 1 [2..]

--FALTA PONER ARITHMOI
--primerTriangular500div :: Maybe Int
--primerTriangular500div = find (\x -> tau (toInteger x) > 500) (listaTriangulares')

{-problema 13-}

{-
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   print $ sum $ (leerInts . splitOn "\n") content
-}

-- normalmente se hace read "846195673" :: Int ó read "5232.488647" :: Float
-- para pasar de un tipo a otro con un map usar una función con cabecera explícita
leerInts ::[String] -> [Integer]
leerInts = map read

{-problema 14-}

secuenciaCollatz :: Int -> [Int]
secuenciaCollatz n
  | (n == 1) = [1]
  | (even n) = n : secuenciaCollatz (par)
  | (odd n)  = n : secuenciaCollatz (impar)
  where
    par = n `div` 2
    impar = 3*n + 1

numMayorCollatz = find (\y -> length (secuenciaCollatz y) == maximalong) [1..999999]
          where maximalong = maximum (map (\x -> length (secuenciaCollatz x))  [1..999999])

{-problema 16-}

sumaCifras :: Integer -> Integer
sumaCifras 0 = 0
sumaCifras a = a `mod` 10 + sumaCifras (a `div` 10)

{-problema 17-}

--MUY mejorable, pero funciona :)

intALetras :: Int -> String
intALetras n
  | (n < 1) = ""
  | (n == 1) = "one"
  | (n == 2) = "two"
  | (n == 3) = "three"
  | (n == 4) = "four"
  | (n == 5) = "five"
  | (n == 6) = "six"
  | (n == 7) = "seven"
  | (n == 8) = "eight"
  | (n == 9) = "nine"
  | (n == 10) = "ten"
  | (n == 11) = "eleven"
  | (n == 12) = "twelve"
  | (n == 13) = "thirteen"
  | (n == 14) = "fourteen"
  | (n == 15) = "fifteen"
  | (n == 16) = "sixteen"
  | (n == 17) = "seventeen"
  | (n == 18) = "eighteen"
  | (n == 19) = "nineteen"
  | (n == 900) = "ninehundred"
  | (n == 800) = "eighthundred"
  | (n == 700) = "sevenhundred"
  | (n == 600) = "sixhundred"
  | (n == 500) = "fivehundred"
  | (n == 400) = "fourhundred"
  | (n == 300) = "threehundred"
  | (n == 200) = "twohundred"
  | (n == 100) = "onehundred"
  | (n == 1000) = "onethousand"
  | (n >= 900) = "ninehundred" ++ "and" ++ intALetras (n - 900) --los and estaban rodeados de espacios
  | (n >= 800) = "eighthundred" ++ "and" ++ intALetras (n - 800)
  | (n >= 700) = "sevenhundred" ++ "and" ++ intALetras (n - 700)
  | (n >= 600) = "sixhundred" ++ "and" ++ intALetras (n - 600)
  | (n >= 500) = "fivehundred" ++ "and" ++ intALetras (n - 500)
  | (n >= 400) = "fourhundred" ++ "and" ++ intALetras (n - 400)
  | (n >= 300) = "threehundred" ++ "and" ++ intALetras (n - 300)
  | (n >= 200) = "twohundred" ++ "and" ++ intALetras (n - 200)
  | (n >= 100) = "onehundred" ++ "and" ++ intALetras (n - 100)
  | (n >= 90) = "ninety" ++ "" ++ intALetras (n - 90) --aquí iban guiones
  | (n >= 80) = "eighty" ++ "" ++ intALetras (n - 80)
  | (n >= 70) = "seventy" ++ "" ++ intALetras (n - 70)
  | (n >= 60) = "sixty" ++ "" ++ intALetras (n - 60)
  | (n >= 50) = "fifty" ++ "" ++ intALetras (n - 50)
  | (n >= 40) = "forty" ++ "" ++ intALetras (n - 40)
  | (n >= 30) = "thirty" ++ "" ++ intALetras (n - 30)
  | (n >= 20) = "twenty" ++ "" ++ intALetras (n - 20)
  | otherwise = error "Está mal"

letrasTotales = length (concat (map (\x -> intALetras x) [1..1000]))

--quitarEspaciosYGuiones :: String -> String

{-problema 20-}

cifrasFact = sumaCifras (product [1..100])

{-problema 21-}

sumaDivisoresPropios :: Int -> Int
sumaDivisoresPropios n = sum $ 1 : [x | x <- [2..n-1], n `mod` x == 0]

esNumeroAmigo :: Int -> Bool
esNumeroAmigo n = n /= posibleAmigo && n == sumaDivisoresPropios posibleAmigo
  where
    posibleAmigo = sumaDivisoresPropios n

sol21 = sum [x | x <- [1..9999], esNumeroAmigo x]

{-problema 23-}

{-
divisoresPropios n = filter (\x -> (n `mod` x) == 0) [1..n `div` 2]

esAbundante = ap (<) (sum . divisoresPropios)

noEscribible n = null [x | x <- todos, esAbundante (n-x)]
  where
    todos = filter esAbundante [12..n-12]

solucion23 = sum $ filter (noEscribible) [1..20161]

main = print solucion23
-}

solucion23 = print . sum $ filter (not . test) [1..28123]

test n = or [(abundant A.! a) && (abundant A.! (n-a)) | a <- [1..div n 2]]

abundant = A.listArray (0,max) $ map isAbundant [0..max] where max = 28123

isAbundant n = n < (sum $ divisors n)

divisors n = 1 : (nub $ facs ++ (map (div n) . reverse) facs)
  where
    facs = filter ((== 0) . mod n) [2..sqrt' n]
    sqrt' n = (floor . sqrt . fromIntegral) n

main = solucion23