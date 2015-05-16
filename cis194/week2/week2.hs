strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

-- los nombres del where sólo valen para la segunda ecuación
frob :: String -> Char
frob [] = 'a'
frob str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where
    len = strLength str

sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums
  where go :: Int -> [Int] -> Int
        go acc [] = acc
        go acc (x:xs)
          | acc >= 20 = acc
          | otherwise = go (acc+x) xs

notEmpty :: [a] -> Bool
notEmpty []    = False
notEmpty (_:_) = True

strange :: a -> b
strange = error "imposible de definir"

limited :: a -> a
limited x = x
-- limited = id

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

addOneToAll :: [Int] -> [Int]
addOneToAll []     = []
addOneToAll (x:xs) = (x + 1) : addOneToAll xs

absToAll :: [Int] -> [Int]
absToAll []     = []
absToAll (x:xs) = abs x : absToAll xs

squareAll :: [Int] -> [Int]
squareAll []     = []
squareAll (x:xs) = x^2 : squareAll xs

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = case p x of
                    True -> x : filter' p xs
                    _    -> filter' p xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

{-
*Main> myFoldl (\x y -> x+y) 0 [1..50]
1275
*Main> sum [1..50]
1275
*Main> myFoldl (\x y -> x*y) 1 [1..50]
30414093201713378043612608166064768844377641568960512000000000000
*Main> product [1..50]
30414093201713378043612608166064768844377641568960512000000000000
*Main> myFoldl (\x _ -> x+1) 0 [1..50]
50
*Main> myFoldl (\x y -> x+y) 0 [1..50]
1275
*Main> sum [1..50]
1275
-}

-- para más adelante, implementar foldr

-- (.) :: (b -> c) -> (a -> b) -> a -> (a -> c)
-- (.) f g x = f (g x)

add1Mult4 :: [Int] -> [Int]
add1Mult4 = map ((*4) . (+1))

-- el dólar se parsea como u operador, y es útil para evitar poner paréntesis
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

negateNumEvens1 :: [Int] -> Int
negateNumEvens1 xs = negate (length (filter even xs))

negateNumEvens2 :: [Int] -> Int
negateNumEvens2 xs = negate $ length $ filter even xs

-- lambdas
duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

-- currying and partial application
-- las flechas de función asocian a la derecha
f :: Int -> Int -> Int
f x y = 2*x + y

-- esta función es equivalente a f
f' :: Int -> (Int -> Int)
f' x y = 2*x + y

-- la aplicación de funciones asocia a la izquierda
-- por tanto f 3 2 es realmente (f 3) 2

{-
The “multi-argument” lambda abstraction

\x y z -> ... 
is really just syntax sugar for

\x -> (\y -> (\z -> ...)).  
Likewise, the function definition

f x y z = ... 
is syntax sugar for

f = \x -> (\y -> (\z -> ...)).
-}

-- If we want to actually represent a function of two arguments we can use a single argument which is a tuple. That is, the function

f'' :: (Int,Int) -> Int
f'' (x,y) = 2*x + y
-- can also be thought of as taking “two arguments”, although in another sense it really only takes one argument which happens to be a pair. In order to convert between the two representations of a two-argument function, the standard library defines functions called curry and uncurry, defined like this (except with different names):

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

-- uncurry in particular can be useful when you have a pair and want to apply a function to it. For example:

-- Prelude> uncurry (+) (2,3)
-- 5

-- ordenar los argumentos "de menor a mayor variación" para hacer
-- la aplicación parcial lo más útil posible

-- ver la parte de secciones del capítulo 1

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)). filter (>3)