add :: (Int, Int) -> Int
add (x1,x2) = x1 + x2

--versión curry de la función add
add' :: Int -> (Int -> Int)
add' x y = x + y

--función hecha por mí, declaración de tipos mejorable
add1 :: Int -> Int
add1 x = add' x 1

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

--función hecha por mí, declaración de tipos mejorable
--multiplica x por el cuadrado de y
multiplicaporcuadrado :: Int -> Int -> Int
multiplicaporcuadrado x y = mult x y y 

zeroto :: Int -> [Int]
zeroto n = [0..n]

--ejercicios cap 3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = 2*x

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

{-
curry :: ((a,b)->c) -> a->b->c
curry f a b = f (a,b)
-}

--ejemplo de parámetro función
twice :: (a -> a) -> a -> a
twice f x = f (f x)


