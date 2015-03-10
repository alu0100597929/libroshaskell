--una lambda sana
inc = \x -> x+1

sumaCuadrados :: Integer -> (Integer -> Integer)
sumaCuadrados = \x -> (\y -> x*x + y*y)

--devuelve true si el segundo número es múltiplo del primero
multiploDe :: Integer -> Integer -> Bool
multiploDe p n = n `mod` p == 0

esPar :: Integer -> Bool
esPar n = multiploDe 2 n

--Secciones, aplicar un operador binario con sólo un argumento
alCubo :: Integer -> Integer
alCubo = (^3)

mitad :: Integer -> Integer
mitad = (`div` 2)

--este es un poco especial
inc' :: Integer -> Integer
inc' = (1+)

--Funciones de orden superior
lista :: [Integer -> Integer]
lista = [(\x -> x+1), (+1), dec, (^2)]
  where
    dec x = x - 1

dosVeces :: (Integer -> Integer) -> Integer -> Integer
dosVeces f x = f (f x)

--aproximación numérica de la derivada de una función real
derivada :: (Float -> Float) -> (Float -> Float)
derivada f = \x -> (f (x + h) - f x) / h
  where
    h = 0.0001

logEnBase :: Float -> (Float -> Float)
logEnBase b = \x -> log x / log b

factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n >= 1 = n * factorial (n-1)

--sumatorio (devuelve la suma de los primeros n naturales)
sumatorio :: Int -> Int
sumatorio 0 = 0
sumatorio n = (+) n $ sumatorio (n-1)

{-combinadores-}
iter :: (Integer -> a -> a) -> a -> Integer -> a
iter op e 0 = e
iter op e n = op n $ iter op e (n-1)

--ahora redefinimos las funciones factorial y sumatorio usando el combinador
factorial' :: Integer -> Integer
factorial' = iter (*) 1

sumatorio' :: Integer -> Integer
sumatorio' = iter (+) 0

--Ejercicio 3.7 escribir la función potencia haciendo uso del combinador iter
potencia :: Integer -> Integer -> Integer
potencia b n = iter (\_ a -> a * b) 1 n
--tutorial: HACER EL DESARROLLO DE ESTO

id' :: a -> a --una variable de tipo, denota cualquier tipo, usan minúsculas
id' x = x

unaVez :: (t1 -> t) -> t1 -> t
unaVez f x = f x

--se usan variables de tipo distintas, con distinto identificador, para permitir que
--el codominio (imagen) de la función f pueda ser distinto del de x

{-Ejercicio 3.8, ¿cuál es el tipo más general de la función dosVeces?-}

dosVeces' :: (a -> a) -> a -> a 
dosVeces' f x = f $ f x 

--composición de funciones:

sumayMult :: Integer -> Integer
sumayMult = (*10) . (+1)

esImpar :: Integer -> Bool
esImpar n = not $ (esPar n) 

unaVez' = id

flip' :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip' f x y = f y x

concatInverso :: [a] -> [a] -> [a]
concatInverso = flip (++)

fun :: Integer -> Integer -> Integer
fun x y = 2*x + y

f1 = fun 3 --fija x a 3, por tanto suma 6 al y que se le pase

f2 = flip fun 10 --fija y a 10, por tanto duplica y suma 10 al x que se le pase

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

{-Ejercicio 3.14 Analizar el tipo más general de las siguientes funciones
	cont x y = x
	subst f g x
	flip f x y = f y x
	curry f x y = f (x, y)
	uncurry f (x y) = f x y
	pair (f, g) x = (f x, g x)
	cross (f, g) (x,y) = (f x, g y)
-}

--iteradores polimórficos sobre los naturales permiten construir estructuras
listaDecre :: Integer -> [Integer]
listaDecre = iter (:) []

palos :: Integer -> String
palos = iter (\n xs -> 'I':xs) []