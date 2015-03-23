factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

--sólo vale para enteros
producto :: Int -> Int -> Int
producto _ 0 = 0
producto a b = a + producto a (b-1)

{-
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs
-}

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

append :: [a] -> a -> [a]
append [] y = [y]
append (x:xs) y = x:append xs y

--a function that inserts a new element of any ordered type into a
--sorted list to give another sorted list can be defined as follows:

insert' :: Ord a => a -> [a] -> [a]
insert' y [] = [y]
insert' x (y:ys) | x <= y = x:y:ys 
                 | otherwise = y:insert' x ys

--ordenación por inserción (insertion sort) se trata de insertar la ca-
--beza de una lista en la cola ordenada con isort (hace uso de la funci-
--ón insert')
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)
--cuidado con no poner paréntesis, si no se ponen, Haskell falla al com-
--pilar

--en cada llamada a isort lo que hacemos es ir aplicando insert recursi-
--vamente (parentizando por la derecha) y se van resolviendo los parén-
--tesis te derecha a izquierda

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

{-
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs
-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--x hace referencia al pivote
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]

--smaller y larger deben usar variables distintas en su list comprehension

--Recursividad Mutua

even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

--funciones que sacan los elementos pares e impares de una lista
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

{-
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs
-}

--el patrón recursivo que sigue product' se resume con la función de 
--librería foldr

--por definición, el producto de 0 enteros es 1, elemento neutro
product' :: Num a => [a] -> a
product' = foldr (*) 1

drop' :: Integral a => a -> [b] -> [b]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs --mejorar esto para que no admita n negativo

init' :: [a] -> [a]
init' [] = error "No se puede hacer init a la lista vacía"
init' [_] = []
init' (x:xs) = x:init' xs

{-Ejercicios-}

exponente :: Int -> Int -> Int
exponente n 0 = 1
exponente n 1 = n
exponente n e = n * exponente n (e - 1)

--decide si todos los valores de una lista son True

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--concatenar una lista de listas

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
--en este tipo de funciones hay que usar el ++, no vale el :

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x: replicate' (n-1) x

--seleccionar el elemento enésimo de una lista (incluso la del Prelude)
--da excepción con la lista vacía
enesimo :: [a] -> Int -> a
enesimo (x:xs) 0 = x
enesimo (x:xs) n = enesimo xs (n - 1)

--decidir si un valor es elemento de una lista
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = (x==y) || elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then x: merge xs (y:ys) else y:merge (x:xs) ys 

halve :: [a] -> [([a], [a])]
halve xs = [(take n xs, drop n xs)]
           where n = (length xs `div` 2)
     
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort mitad1) (msort mitad2)
     where mitad1 = fst (head mitad)
           mitad2 = snd (head mitad)
           mitad = halve xs