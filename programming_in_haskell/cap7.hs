import Data.List
import Data.Char --ord

--función de orden superior suma, implementada usando lambdas
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

--ahora sin lambdas se ve claramente en los argumentos que las funciones
--se pasan como entre paréntesis
twice :: (a -> a) -> a -> a
twice f a = f (f a)

--recuerda, estas funciones no reciben nada explícitamente, los parámetr
--os los rellenamos en la curried en la que se basan, y luego se llaman
--con los parámetros que faltan
quadruple = twice (*2)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x: map' f xs

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' f xs = [x | x <- xs, f x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if (f x) then x:filter' f xs else filter' f xs

--ns es una lista de números en la notación típica del libro
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

--la función foldr (fold right) resume patrones recursivos como:
product' [] = 1
product' (x:xs) = x * product' xs 

--en cosas como:
productfold :: [Int] -> Int
productfold = foldr (*) 1
--notar que el operador debe estar parentizado porque se usa como argum.

--es decir foldr (+) 0 se puede leer como, suma todos los elementos de 
--la lista, y cuando llegues a lista vacía sumas 0

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

sumfold = foldr' (+) 0

--creada por mí, mejorada por la del libro
--length' :: [a] -> Int
--length' xs = foldr' (+) 0 [1 | x <- xs]

{-
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs
-}

length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

--hemos usado la definición anteriormente comentada, y hemos hecho un 
--foldr donde la función es una lambda que suma 1 a su 2º argumento (el
--1º da igual) y cuyo caso base de lista vacía es 0

{-
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-}

--definimos la función intermedia snoc que añade un elemento al final
--de una lista, snoc es "cons" al revés
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

{-
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = snoc x (reverse' xs)
-}

--reverse' [1..3] daría: snoc 1 (snoc 2 (snoc 3 (snoc []))) y luego, re-
--solviendo los paréntesis de la derecha primero todo quedaría en el or-
--den correcto, es decir, al revés que al principio
--por tanto foldr se usa para operadores que asocian a DERECHAS

reverse' :: [a] -> [a]
reverse' = foldr snoc []

--recuerda, el primer parámetro es sólo la operación que se hace a cada
--elemento, sin especificar parámetros ni nada

sum' = sum'' 0
       where
           sum'' v [] = v
           sum'' v (x:xs) = sum'' (v + x) xs

--ahora vemos la suma como una operación que asocia a IZQUIERDAS, y por
--ello vamos haciendo cada vez más pequeña la lista sumando cada elemen
--to al acumulador, que empieza en 1
--es decir, FOLDL es parecido a FOLDR pero a izquierdas, y usa el acum
--mulador con el valor que le pasemos

suma :: (Num a) => [a] -> a
suma = foldl (+) 0

lengthfoldl :: [a] -> Int
lengthfoldl = foldl (\n _ -> n + 1) 0

reversefoldl :: [a] -> [a]
reversefoldl = foldl (\xs x -> x : xs) []

{- aún NO entiendo estas lambdas
lambda1 = (\n _ -> n + 1)
lambda2 = (\xs x -> x : xs)
-}

--Composición de funciones: el resultado de la segunda (o última por la
--derecha) se le pasa con una tubería a la de más a la izq. y así sucesi
--vamente

--HAY QUE ARREGLAR ESTO
--ordenacion_descendente = reverse . sort

impares = not . even
twice' f = f . f
sumsqreven' = sum . map (^2) . filter even

id' :: a -> a
id' = \x -> x

--de una lista de funciones saco una función
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w , b) <- zip weights bits]
               where weights = iterate (*2) 1

--ghci> take 5 (iterate (*2) 1)
--[1,2,4,8,16]
--por tanto iterate da una lista que empieza en el valor que le paso co
--mo segundo parámetro, y en cada posición se le aplica al valor inicial
--la función el número de veces igual a la posición en la lista
--iterate f x = [x , f x , f (f x ), f (f (f x )), · · · ]

--funciona sobre listas de bits al revés! [0,0,0,1] sería 8!!
bin2int' = foldr (\x y -> x + 2 * y) 0

lambda1 = (\_ n -> 1 + n)

--esta versión del reverse es más lenta porque usar (++) en vez de (:)
reversefoldr :: [a] -> [a]
reversefoldr = foldr (\x xs -> xs ++ [x]) []

lengthfoldl' :: [a] -> Int
lengthfoldl' = foldl (\n _ -> 1 + n) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = [n `mod` 2] ++ int2bin (n `div` 2) 

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

--cuidado, ord vale para cualquier cosa, String del tipo que sea, pero
--toEnum sólo vale para Char
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

--recuerda: map hace la operación que quieras a todos los elementos de 
--una lista (ord cambia de Unicode a número)
mapear = map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = take 8 xs: chop8 (drop 8 xs)

--chr cambia de número a Unicode
decode :: [Bit] -> String
decode = map (chr . bin2int') . chop8

transmit :: String -> String
transmit  = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

{-Ejercicios capítulo 7-}

--expresar [f x | x <- xs, p x] con map y filter

--OJO con esta cabecera, vemos que en esta función el tipo de dato no
--cambia!
filtro :: (a -> a) -> (a -> Bool) -> [a] -> [a]
filtro f p xs = map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) = if (p x) then x:takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs) = if (p x) then dropWhile' p xs else x:xs

--rehacer map y filter usando foldr
mapfoldr :: (a -> b) -> [a] -> [b]
mapfoldr f = foldr (\x xs -> f x : xs) []

filterfoldr :: (a -> Bool) -> [a] -> [a]
filterfoldr p = foldr (\x xs -> if (p x) then x:xs else xs) [] 

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2binunfold = unfold (== 0) (`mod` 2) (`div` 2)
--es decir, predicado p es la condición de parada
--h es lo que se le va haciendo a cada elemento de la lista para ir aña-
--diendo.
--t se hace recursivamente y nos va dando el elemento para aplicarle h

{-
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = take 8 xs: chop8 (drop 8 xs)
-}

chop8unfold = unfold (null) (take 8) (drop 8)

{-
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x: map' f xs
-}

mapunfold f = unfold (null) (f . head) (tail)

iterateunfold f x = unfold (null) (f) (f x)

paritybit :: [Bit] -> Bit
paritybit x
  | odd (n_unos) = 1
  | otherwise    = 0
  where n_unos = sum (filter (== 1) x)

appendparity :: [Bit] -> [Bit]
appendparity xs = xs ++ [paritybit xs]

encodeparity :: String -> [Bit]
encodeparity = concat . map (appendparity . make8 . int2bin . ord)

checkparity :: [Bit] -> [Bit]
checkparity xs = if xs == appendparity (principio) then principio else error "Error de paridad"
                 where principio = init xs

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 xs = take 9 xs : chop9 (drop 9 xs)

--FALTA implementar esta función
decodeparity :: [Bit] -> String
decodeparity = map (chr . bin2int' . checkparity) . chop9

--introduce un canal que sólo coge la cola, por tanto hacemos fallar a
--la función deliberadamente para comprobar que salta el error
transmitfail :: String -> String
transmitfail  = decodeparity . tail . encodeparity