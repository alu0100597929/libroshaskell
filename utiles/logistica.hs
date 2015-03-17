import Data.List
import Data.Char(toUpper)

letraDNI :: Int -> Char
letraDNI n = ['T','R','W','A','G','M','Y','F','P','D','X','B','N','J','Z','S','Q','V','H','L','C','K','E'] !! (n `mod` 23)

fact 0 = 1
fact n = n * fact (n-1)

factorial :: Integer -> Integer
factorial n
  | n < 0  = error "no existe el factorial para enteros negativos"
  | n == 0 = 1
  | otherwise = n * factorial (n-1)

factorial' :: Integer -> Integer
factorial' n = product [1..n]

factorial'' :: Integer -> Integer
factorial'' n = foldl (*) 1 [1..n]

--123 [1,2,3]
intALista' :: Int -> [Int]
intALista' 0 = []
intALista' n = flip (:) (intALista' (n `div` 10)) (n `mod` 10)

intALista :: Int -> [Int]
intALista = reverse . intALista'

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + (suma xs)

len :: [a] -> Int
len [] = 0
len (_:xs) = len xs + 1

last' :: [a] -> a
last' [] = error "Lista vacia"
last' [x] = x
last' (_:xs) = last' xs

penultimo :: [a] -> a
penultimo [] = error "Lista vacia"
penultimo [_] = error "No hay penultimo"
penultimo [x, _] = x
penultimo (_:xs) = penultimo xs

nth :: [a] -> Int -> a
nth [] _ = error "No existe"
nth (x:_) 1 = x
nth (_:xs) n = nth xs (n-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

compresion = [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a^2 + b^2 == c^2]

sumar :: Int -> Int -> Int
sumar a b = a + b

sumar18 = (+18)

--fin clase, aunque no la incluye toda

esPalindroma :: (Eq a) => [a] -> Bool
esPalindroma xs
  | null xs = True
  | head xs == last xs = esPalindroma (if not (null (init xs))
                                          then tail (init xs)
                                          else [])
  | otherwise = False

posibles = [a:[b] | a <- ['a','b','c'], b <- ['a','b','c']]

posAlfabeto :: Char -> Int
posAlfabeto c = case findIndex (== (toUpper c)) alfabeto of Just x -> succ x
                                                            Nothing -> -1
  where
    alfabeto = ['A'..'M'] ++ ['Ñ'] ++ ['O'..'Z']