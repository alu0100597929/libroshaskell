--Todas las funciones de Haskell son "curried" por defecto
--Las funciones de orden superior se asocian:
--En su llamada: a izquierdas
--En sus parámetros: a la derecha

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

--el primer parámetro está "fijado" por tanto faltarían los otros dos,
--que se los debemos pasar a multTwoWithNine
multTwoWithNine = multThree 9

--pasa el primer parámetro de compare
compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

--en estas 3 funciones, fijamos el primer parámetro
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

--aquí se usa subtract 4 porque (-4) sería el número "menos 4" y no la
--operación de restar 4 de otra cantidad
restarCuatro :: Int -> Int
restarCuatro = (subtract 4)

--recuerda: para saber que los parámetros son funciones, se ponen entre
--paréntesis
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--It takes a function and two lists as parameters, and then joins the
--two lists by applying the function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--Atención! Los parámetros de la función anterior dicen que a b y c 
--PODRÍAN ser de distintos tipos, pero no tienen por qué serlo!

--Truco: cuando no se sepa de qué tipo es una función de orden superior,
--se programa sin tipo, se compila y luego con :t vemos qué interpretó ghci

--la flecha asocia a derechas por defecto
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger         = filter (> x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

--buen truco!!    
largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

findSum :: Integer
findSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n  = n:chain (n*3 + 1)
    
numCadenasCollatz :: Int
numCadenasCollatz = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numCadenasCollatz' :: Int
numCadenasCollatz' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

--este ejemplo ilustra qué parámetro es el acumulador en el caso de foldl
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs --foldl (+) 0 xs

--este ejemplo ilustra qué parámetro es el acumulador en el caso de foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
--más eficiente que usar foldl, ya que requeriría usar "++"

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if y == x then True else acc) False ys
--funciona para listas vacías porque los plegados con listas vacías devuelven el valor inicial del acumulador

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []--foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

--versión mía
last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

--versión libro
last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True

--How many elements does it take for the sum of the square roots of all
--natural numbers to exceed 1,000? Usando scans
num_elementos = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

{-
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]
ghci> scanr (+) 0 [3,5,2,1]
[11,8,3,1,0]
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]
ghci> scanl (flip (:)) [] [3,2,1]
[[],[3],[2,3],[1,2,3]]
-}

--función escrita al estilo clock-wise
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

--función usada para esclarecer la aplicación de composiciones
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]