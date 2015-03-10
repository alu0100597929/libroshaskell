import Data.List (foldl')

-- Version 1
{-evenSum :: [Integer] -> Integer

evenSum l = accumSum 0 l

accumSum n l = if l == []
                  then n
                  else let x = head l
                           xs = tail l
                       in if even x
                              then accumSum (n+x) xs
                              else accumSum n xs
main = print $ evenSum [1..10]-}

{-Los where sirven para definir subprogramas-}
-- Version 2 
{-evenSum' :: Integral a => [a] -> a

evenSum' l = accumSum 0 l
     where accumSum n l =
            if l == []
                then n
                else let x = head l
                         xs = tail l
                     in if even x
                            then accumSum (n+x) xs
                            else accumSum n xs
main = print $ evenSum' [1..10]-}

-- Version 3
{-evenSum'' l = accumSum 0 l
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs

main = print $ evenSum'' [1..10] -}

foo n (x:xs) = if even x
                 then foo (n+x) xs
                 else foo n xs

{-
Esto es una n-reducción

f x = (some expresion) x
f = (some expresion)
-}

--ejercicio: Version 4 n-reducida
{-evenSum''' = accumSum 0
    where
        accumSum n [] = n
        accumSum n (x:xs) =
             if even x
                then accumSum (n+x) xs
                else accumSum n xs-}

-- Version 5
{-evenSum l = mysum 0 (filter even l)
    where
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs-}

--main = print $ evenSum [1..10]

-- Version 6
-- foldl' isn't accessible by default
-- we need to import it from the module Data.List
{-evenSum l = foldl' mysum 0 (filter even l)
  where mysum acc value = acc + value-}

 --main = print $ evenSum [1..10]

-- Version 7
-- Generally it is considered a good practice
-- to import only the necessary function(s)
evenSum' l = foldl' (\x y -> x+y) 0 (filter even l)

--main = print $ evenSum [1..10]

-- Version 8
{-evenSum'' :: Integral a => [a] -> a
evenSum'' l = foldl' (+) 0 (filter even l)-}

--main = print $ evenSum'' [1..10]

--Truco, usar n-reducción para hacer que el tercer parámetro de fold' (la lista)
--sea el de prod (no se escribe ninguno)
prod :: [Integer] -> Integer
prod = foldl' (*) 1

--main = print $ prod [3,4,5]

-- Version 9
evenSum'' :: Integral a => [a] -> a
evenSum'' = foldl' (+) 0 . (filter even)

-- Version 10
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

evenSum :: Integral a => [a] -> a
evenSum = sum' . (filter even)

--main = print $ evenSum [1..10]

--originalmente: squareEvenSum = sum' . (filter even) . (map (^2))
squareEvenSum = sum' . (map (^2)) . (filter even) --creo que es mejor
squareEvenSum' = evenSum . (map (^2))

main = print $ squareEvenSum [1..10]