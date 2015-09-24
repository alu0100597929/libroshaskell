import Data.Char

-- código del libro

roots :: (Float,Float,Float) -> (Float,Float)
roots (a,b,c)
  | a == 0 = error "not quadratic"
  | disc < 0 = error "complex roots"
  | otherwise = ((-b-r)/e, (-b+r)/e)
  where disc = b*b - 4*a*c
        r    = sqrt disc
        e    = 2*a

-- EXERCISES

-- A

half :: Int -> Int
half n = n `div` 2

puzzle :: Int
puzzle = half 2 + 2

-- C

cadena = "The morphology of prex - an essay in meta-algorithmics"

modernise :: String -> String
modernise str = unwords . map (\(x:xs) -> toUpper x : xs) $ words str

-- D

-- una función preparada para eager evaluation que devuelve el primer elemento de una lista que cumple el predicado

first :: (a -> Bool) -> [a] -> a
first p xs
  | null xs = error "Empty list"
  | p x = x
  | otherwise = first p $ tail xs
  where x = head xs

-- E

firstMaybe :: (a -> Bool) -> [a] -> Maybe a
firstMaybe p xs
  | null xs = Nothing
  | p x = Just x
  | otherwise = firstMaybe p $ tail xs
  where x = head xs

-- F

exp' :: Integer -> Integer -> Integer
exp' x n
  | n == 0 = 1
  | n == 1 = x
  | even n = simplificacionPares * simplificacionPares
  | odd n = x * (x `exp'` ((n-1) `div` 2))
  where simplificacionPares = x `exp'` (n `div` 2)