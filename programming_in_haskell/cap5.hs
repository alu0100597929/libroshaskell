import Data.Char --isLower...etc

--cuanto más a la derecha está el generador, más "anidado" está, por
--tanto sus "variables" cambiarán más frecuentemente
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

--puedo usar pattern matching en los generadores!!
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps] --ps significa lista de pares, por conven-
			      --ción

--puedo cambiar los elementos "mapeados" por el generado por lo que yo
--quiera, números por ejemplo
length' :: [a] -> Int
length' xs = sum [1 |_ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

--esta función para, devolviendo false desde que la función factors pro-
--duce una lista que no sea [1,n] debido a la evaluación perezosa 
primo :: Int -> Bool
primo n = factors n == [1,n]

primos :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]

--encuentra todos los pares de una lista que coinciden con la clave
find' :: (Eq a) => a -> [(a,b)] -> [b]
find' k t = [v | (k',v) <- t, k == k']

--devuelve todos los pares de elementos adyacentes en una lista dada
pairs :: [a ] -> [(a, a)]
pairs xs = zip xs (tail xs)

--función sorted hace una and de toda la lista!!!
sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x<=y |(x,y) <- pairs xs]

--esta función sorted para desde que un par no esté ordenado, por lo 
--cual pairs sólo produce pares mientras la condición se cumple

{-Using zip we can also define a function that returns the list of all positions
at which a value occurs in a list, by pairing each element with its position,
and selecting those positions at which the desired value occurs:-}

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
		 where n = length xs - 1
--este método está copiado del libro y la notación que emplea es muy
--ilustrativa, es decir, los índices se llaman i, se usa x y x'

--devuelve el número de minúsculas en una cadena dada
lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, x `elem` ['a'..'z']]

count :: (Eq a) => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

--ya no se usan las funciones ord y chr, en su lugar se usan fromEnum y
--toEnum, respectivamente

--estas 2 funciones le dan a cada letra del alfabeto inglés su correspon
--diente número, (del 0 al 25)
let2int :: Char -> Int
let2int c = fromEnum c - fromEnum 'a'

int2let :: Int -> Char
int2let n = toEnum (fromEnum 'a' + n)

--desplaza el carácter c con un factor de f (cifrado César)
shift :: Int -> Char -> Char
shift f c | isLower c = int2let ((let2int c + f) `mod` 26)
          | otherwise = c

encode :: Int -> [Char] -> [Char]
encode f xs = [shift f x| x <- xs]

--el cifrado césar se decodifica usando un factor negativo

--fromIntegral pasa de Int a Float
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs
--importante: el uso del where hace que sólo se calcule una vez

--función que calcula la chi-cuadrado entre 2 listas de frecuencias
--cuanto más pequeña es la chi, más se parecen las listas
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

--buen truco, crear pares (o tuplas) cuando simplemente queramos coger
--elementos n-ésimos de listas diferentes

{-we define a function that rotates the elements of a list n places
to the left, wrapping around at the start of the list, and assuming that
n is between zero and the length of the list-}

rotate :: Int -> [a] -> [a]
rotate f xs = drop f xs ++ take f xs

--la lista de las frecuencias más típicas en inglés de todas las letras
--del alfabeto
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

--esta es la tabla para el español, sacada de wikipedia
tabla = [12.53, 1.42, 4.68, 5.86, 13.6, 0.69, 1.01, 0.70, 6.25, 0.44, 0.01, 4.97, 3.15, 6.71, 0.31, 8.68, 2.51, 0.88, 6.87, 7.98, 4.63, 3.93, 0.90, 0.02, 0.22, 0.90, 0.52]
--se podría modificiar la función crack para crackear el español, pero
--habría que tener en cuenta el caso de la ñ

--chitab hace la chi cuadrado para todas las rotaciones posibles
crack :: String -> String
crack xs = encode (-factor) xs
           where factor = head (positions (minimum chitab) chitab)
                 chitab = [chisqr (rotate n table') table | n <- [0..25]]
                 table' = freqs xs

{-ejercicios-}

sqrsum :: Int
sqrsum = sum [x^2|x <- [1..100]]

replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int )]
pyths n = [(x,y,z)| x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2] 

--un número es perfecto si es igual a la suma de sus factores
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == (sum (factors x) - x)]

--combinaciones = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

--nested list comprehensions, dan una lista de listas
nested = [[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]
--luego se concatenan y dan la lista pedida, en otro orden pero da igual
combinations = concat [[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]

--volver a codificar positions haciendo uso de find'
positions2 :: (Eq a) => a -> [a] -> [Int]
positions2 v vs = find' v (zip vs [0..n])
                  where n = (length vs) - 1
		  
--not (null (cosa_que_puede_dar_null))

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y |(x,y) <- zip xs ys] 

--OJO: siempre que queramos elementos en plan (1º,1º),(2º,2º) es decir,
--primero de la primera lista con el primero de la segunda, etc. debemos
--usar zip