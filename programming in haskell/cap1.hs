double x = x + x

--lo que va antes de la => indica con qué tipos trabaja la función
--lo de después indica que lo de la derecha del todo es lo que devuelve
--(sólo una cosa) y lo de la izq. son los argumentos que recibe
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--quicksort implementado con where
qsort :: Ord a => [a ] -> [a ]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

--ejercicios
double' :: Num a => a -> a
double' x = 2*x

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

--modificacion del qsort para que la lista esté ordenada al revés
qsort' :: Ord a => [a ] -> [a ]
qsort' [] = []
qsort' (x : xs) = qsort' larger ++ [x] ++ qsort' smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
	
--si le quitamos el <= al qsort y ponemos sólo menor, se carga las repe-
--ticiones de los números, pero ordena bien

--cap2
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

{-Ejercicios cap 2-}

n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]
	
--last' xs = head (reverse xs)

--last' (x:xs) = if xs ==[] then x else last' xs

last' (x:[]) = x
last' (x:xs) = last' xs

--init' xs = take (length xs - 1) xs

init' (x:[]) = []
init' (x:xs) = x:init' xs


