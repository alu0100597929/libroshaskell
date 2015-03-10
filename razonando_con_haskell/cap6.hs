import Data.Char
import Data.List
import Data.Ord --comparing, método usado para tuplas

--La clase Enum. Sus elementos tienen orden, es coherente que vaya uno después de otro

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show, Enum)

data Nat = Cero | Suc Nat deriving Show --representa los números naturales, en plan primitiva recursiva

iter :: (Int -> a -> a) -> a -> Int -> a
iter op e 0 = e
iter op e n = op n $ iter op e (n-1)

foldNat :: (a -> a) -> a -> Nat -> a
foldNat f e Cero = e
foldNat f e (Suc n) = f (foldNat f e n)

instance Enum Nat where
  toEnum = iter (\ _ q -> Suc q) Cero --hay que decirle que queremos un Nat (toEnum 5 :: Nat)
  fromEnum = foldNat (+1) 0

--ejercicio 6.1

--infix 5 TODO
{-conc :: [a] -> [a] -> [a]
conc xs [] = xs
conc xs (y:ys) = xs ++ (conc y ys)-}

--infixr 5 >++>
(>++>) :: [a] -> a -> [a]
[] >++> y = [y] --error común, intentar devolver y cuando debe devolverse [y]
(x:xs) >++> y = x : (xs >++> y)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

last' :: [a] -> a
last' [] = error "no hay último en las listas vacías"
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "no hay principio en las listas vacías"
init' [x] = []
init' (x:xs) = x : init' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs

listaAlCuadrado :: [Int] -> [Int]
listaAlCuadrado []     = []
listaAlCuadrado (x:xs) = alCuadrado x : listaAlCuadrado xs
                         where
                           alCuadrado = (^2)

listaAMayusculas :: [Char] -> [Char]
listaAMayusculas []     = []
listaAMayusculas (x:xs) = toUpper x : listaAMayusculas xs

--es mejor usar map que definir funciones casi iguales

--ejercicio 6.16 pág 144
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if (p x) then
                                   dropWhile' p xs
                               else x:xs

divideA :: Integer -> Integer -> Bool
x `divideA` y = y `mod` x == 0

divisoresDe :: Integer -> [Integer]
divisoresDe n = [x | x <- [1..n], x `divideA` n]

mcd :: Integer -> Integer -> Integer
mcd a b = maximum [x | x <- divisoresDe a, x `divideA` b]

esPrimo :: Integer -> Bool
esPrimo n = divisoresDe n == [1,n]

losPrimos = [x | x <- [2..], esPrimo x]

--ejercicio 6.17
listaPrimosCond = [x | x <- [2..1000], esPrimo x, x `mod` 10 == 3]

primeroQue :: (a -> Bool) -> [a] -> a
primeroQue p [] = error "no hay primero que cumpla nada en una lista vacía"
primeroQue p (x:xs) = if (p x) then x
                               else primeroQue p xs

listaPerfectos = [x | x <- [1..], sum (init (divisoresDe x)) == x]

ternasPitHasta :: Integer -> [(Integer, Integer, Integer)]
ternasPitHasta n = [(x,y,z) | let ns = [1..n],
                              x <- ns, y <- ns, z <- ns,
                              x^2 + y^2 == z^2]

--comprueba si una operación es conmutativa para un cjto de datos dado (una lista)
opConmutativa :: (Eq b) => (a -> a -> b) -> [a] -> Bool
opConmutativa f ns = and [a `f` b == b `f` a | a <- ns, b <- ns]

--Plegado de listas: foldr y foldl

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

concat' :: [[a]] -> [a]
concat' = foldr (++) []

length' :: [a] -> Integer
length' = foldr (\_ n -> n + 1) 0

reverse'' :: [a] -> [a]
reverse'' = foldr (\x xs -> xs ++ [x]) []

--ejercicio 6.22
and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = if (p x)
                  then all' p xs
                  else False

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = if (p x)
                  then True
                  else any' p xs

--ejercicio 6.24
--gracias a la explicación de LYAH, ahora se cómo funciona
maximumfold :: (Ord a) => [a] -> a
maximumfold = foldr1 (\x n -> if x > n
                                then x
                                else n)

minimumfold :: (Ord a) => [a] -> a
minimumfold = foldr1 (\x n -> if x < n 
                                then x
                                else n)

listaFactoriales n = tail $ scanl (*) 1 [1..n]

ordenadaPor :: (a -> a -> Bool) -> [a] -> Bool
ordenadaPor cond [] = True
ordenadaPor cond (x:[]) = True
ordenadaPor cond (x:y:xs) = if (x `cond` y)
                              then ordenadaPor cond (y:xs)
                              else False

insertar :: (Ord a) => a -> [a] -> [a]
insertar x [] = [x]
insertar x ls@(y:ys)
  | x <= y    = x : ls
  | otherwise = y : insertar x ys

ordInsercion :: (Ord a) => [a] -> [a]
ordInsercion = foldr insertar []

--mergesort

partir :: [a] -> ([a], [a])
partir xs = (take puntocorte xs, drop puntocorte xs)
            where
              puntocorte = largo `div` 2 + if (even largo) 
                                             then 0
                                             else 1
              largo = length xs

--Si xs e ys son dos listas ordenadas, entonces mezclar xs ys es una lista
--ordenada que incluye todos los elementos de ambas
mezclar :: (Ord a) => [a] -> [a] -> [a]
mezclar li [] = li
mezclar [] ld = ld
mezclar li@(x:xs) ld@(y:ys)
  | x <= y    = x : mezclar xs ld
  | otherwise = y : mezclar li ys

primero :: (a, b) -> a
primero (x,_) = x

segundo :: (a, b) -> b
segundo (_, y) = y

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = mezclar (mergesort (primero par_listas)) (mergesort (segundo par_listas))
               where
                 par_listas = partir xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort mas_pequenios ++ [x] ++ quicksort mas_grandes
                   where
                     mas_pequenios = filter (< x) xs
                     mas_grandes   = filter (>= x) xs

{-partirPor :: (a -> Bool) -> [a] -> ([a], [a])
partirPor cond (x:xs)
  | cond x    = (x:as, bs)
  | otherwise = (as, x:bs)-}

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (x:xs) = [] : map (x:) (inits' xs)

--ejercicio 6.31
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' ls@(x:xs) = ls : tails' xs

segs :: [a] -> [[a]]
segs [] = [[]]
segs ls@(x:xs) = segs xs ++ tail (inits' ls)

--ejercicio 6.32 Defina la función partes que permite calcular los subconjuntos de un conjunto
--borra la posición que queramos de una lista
partes :: [a] -> [[a]]
partes []     = [[]]
partes (x:xs) = partes xs ++ map (x:) (partes xs)

--función mía que inserta un elemento en la posición que queramos de cualquier lista
insertarEnPos :: Int -> a -> [a] -> [a]
insertarEnPos 0 x [] = [x] --línea necesaria por los patrones exhaustivos del compilador
insertarEnPos n x ls@(y:ys)
  | n < 0 || n > long = insertarEnPos long x ls
  | n == 0            = x : ls
  | otherwise         = y : insertarEnPos (n - 1) x ys
  where
    long = length ls

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x ls@(y:ys) = (x:ls) : map(y:) (intercala x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (intercala x) (perms xs))

--ejercicio 6.33
repBin :: Int -> [Int]
repBin n
  | n == 0 || n == 1 = [n]
  | otherwise        = repBin (n `div` 2) ++ repBin (n `mod` 2)

--funciona sobre listas de bits al revés! [0,0,0,1] sería 8!!
repDec :: [Int] -> Int
repDec = foldr (\x y -> x + 2*y) 0

--ejercicio 6.35
inv2 :: [a] -> [a] -> [a]
inv2 [] ys     = ys
inv2 (x:xs) ys = inv2 xs (x:ys)

esCapicua :: (Eq a) => [a] -> Bool
esCapicua xs = xs ++ xs == inv2 xs xs

--ejercicio 6.36
de :: Int -> [a] -> a
n `de` xs = xs !! (n - 1)

--ejercicio 6.39
circulaI :: [a] -> [a]
circulaI []     = []
circulaI (x:xs) = xs ++ [x]

circulaD :: [a] -> [a]
circulaD [] = []
circulaD xs = (last xs) : (init xs)

--ejercicio 6.40 y 6.41 (modificación para que no distinga entre mayúsculas y minúsculas)
type Palabra = String

esMenorQue :: Palabra -> Palabra -> Bool
[] `esMenorQue` _ = False
_ `esMenorQue`[]  = True
(x:xs) `esMenorQue` (y:ys)
  | ord (toUpper x) == ord (toUpper y) = xs `esMenorQue` ys
  | ord (toUpper x) < ord (toUpper y)  = True
  | otherwise = False

--ejercicio 6.42
caracter :: Int -> Palabra -> Char
caracter n xs = xs !! n

--ejercicio 6.43
--no funciona si hay carácteres repetidos, hay que arreglarla
pos :: Char -> Palabra -> Int
pos c xs = case findIndex (\x -> x == c) xs of Just x  -> x
                                               Nothing -> error "El carácter no se encuentra en la palabra"
--ejercicio 6.44
--la posición original de cada carácter en el anagrama1
pos_original :: Palabra -> [Int]
pos_original anagrama1 = map (\x -> pos x anagrama1) anagrama1

--me dice los cambios que tengo que hacer en la cadena original
--n positivo = sumar n a la posición original
--n negativo = restar n a la posición original
cambio_pos :: Palabra -> Palabra -> [Int]
cambio_pos anagrama1 anagrama2 = map (\x -> (pos x anagrama2 - pos x anagrama1)) anagrama1

sumaPar :: (Int, Int) -> Int
sumaPar (x, y) = (+) x y

--por las propiedades de la suma (o resta) de enteros, sumar la posición original
--con el cambio dará la posición final en la que debe estar cada carácter
pos_final :: Palabra -> Palabra -> [Int]
pos_final anagrama1 anagrama2 = map (\x -> sumaPar x) $ zip (pos_original anagrama1) (cambio_pos anagrama1 anagrama2)

mapa :: Palabra -> Palabra -> Palabra -> [(Char,Int)]
mapa fuente anagrama1 anagrama2 = zip fuente (pos_final anagrama1 anagrama2)

--función que ordena una lista de tuplas en base a su segundo elemento
ordenarTuplaGT (a1, b1) (a2, b2)
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = compare a1 a2

-- *Main> ordenar "UVWXYZ" "fedcba" "ecabdf" => "VXZYWU"
ordenar fuente anagrama1 anagrama2 = map (fst) $ sortBy ordenarTuplaGT (mapa fuente anagrama1 anagrama2)

--ejercicio 6.45
aniade :: a -> [a] -> [a]
aniade x ys = ys ++ [x] 

--ejercicio 6.59
--recuerda: concat trabaja con listas de listas
alFinal :: a -> [a] -> [a]
alFinal x ys = concat [ys,[x]]--reverse $ x: reverse ys

invertirLista :: [a] -> [a]
invertirLista = foldr (alFinal) []

--ejercicio 6.60
--TODO
infixr 0 :=>
data Secuencia a = Uno a | a :=> Secuencia a deriving Show