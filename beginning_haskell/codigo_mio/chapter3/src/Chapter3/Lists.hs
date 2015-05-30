module Chapter3.Lists where

import Prelude hiding (foldr, foldl)
import Chapter3.ParamPoly

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax :: (Ord a) => InfNumber a -> InfNumber a -> InfNumber a
infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity _        = PlusInfinity
infMax _ PlusInfinity        = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

maximo = foldr infMax MinusInfinity $ map Number [1,2,3]

maximo' = foldr (\x y -> infMax (Number x) y) MinusInfinity [1,2,3]

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

-- foldr y foldl NO son conmutativos para operaciones binarias que no lo son, como la resta

maximo'' :: [Integer] -> Integer
maximo'' = foldr1 max

-- Exercise 3-3

product' :: (Num a) => [a] -> a
product' []     = error "no se puede calcular el producto de la lista vacía"
product' [x]    = x
product' (x:xs) = x * product' xs

product'' = foldr (*) 1

all' _ []        = True
all' p [x] | p x = True | otherwise = False
all' p (x:xs)    = p x && (all' p xs)

all'' p = foldr (\x -> ((&&) . p) x) True

-- el ejercicios continúa en el fichero main.hs fuera de la carpeta Chapter3