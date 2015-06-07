{-# LANGUAGE LambdaCase #-}

module Chapter3.Origami where

import Data.List(delete)

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold p = foldr (\x acc -> if p x then x:acc else acc) []

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x acc -> (f x):acc) []

unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f seed = case f seed of
                   Nothing        -> []
                   Just (x,seed') -> x : unfoldr f seed'

enumUnfold :: Int -> Int -> [Int]
enumUnfold m n = unfoldr (\x -> if x > n then Nothing else Just (x,x+1)) m

-- una idea muy curiosa, hago el "trabajo" en el unfoldr y de ese modo no tengo
-- que codificar tan duramente otras funciones. Buen modo de generar una lista
-- con semillas

-- obtenemos el mínimo, lo sacamos de la lista y lo ponemos en la lista resultado
minSort :: [Integer] -> [Integer]
minSort = unfoldr (\x -> let min = minimum x
                         in if null x then Nothing else Just (min, filter (/= min) x))

minSort' :: [Integer] -> [Integer]
minSort' = unfoldr (\case [] -> Nothing
                          xs -> Just (m, delete m xs) where m = minimum xs)

foldr2 :: (Maybe (a,b) -> b) -> [a] -> b
foldr2 f []     = f Nothing
foldr2 f (x:xs) = f $ Just (x, foldr2 f xs)
 
mapAsFold2 :: (a -> b) -> [a] -> [b]
mapAsFold2 f = foldr2 (\case Nothing -> []
                             Just (x,xs) -> f x : xs)