{-# LANGUAGE BangPatterns #-} -- seq con ! antes del argumento forzado

-- No tiene que ver cómo está construido su argumento
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

-- Tiene que ver con qué constructor fue construido su argumento
f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

{-
El slogan a recordar es: “el reconocimiento de patrones dirige la evaluación”.

Dos cosas importantes:

Las expresiones sólo son evaluadas cuando se hace el pattern matching.
...sólo lo que sea necesario para efectuar el pattern matching, y no más!
-}

-- la evaluación perezosa nos obliga a emplear la pureza de funciones

-- la función seq a -> b -> b es mágica en el sentido de que no puede ser definida en
-- haskell normal. Lo que hace es forzar la evaluación de su primer argumento
-- antes de devolver el segundo
strictSum :: Num a => [a] -> a
strictSum = go 0
  where go acc []     = acc
        go acc (x:xs) = acc `seq` go (x + acc) xs

strictSum' :: Num a => [a] -> a
strictSum' = go 0
  where go acc [] = acc
        go !acc (x:xs) = go (x + acc) xs

-- tener en cuenta los ejemplos de && y || típicos de la evaluación perezosa
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

withIndices :: [a] -> [(a,Integer)]
withIndices xs = zip xs [0..]

nats :: [Integer]
nats = 0 : map (+1) nats

-- ejemplo de ejecución:

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

{-
el +1 se va haciendo a todas las cabezas de la lista infinita, y cuanto más
a la derecha esté el elemento que queremos calcular, más sumas de 1 se le aplicarán
take 4 nats
take 4 (0 : map (+1) nats)
take 4 (0 : map (+1) (0 : map (+1) nats))
take 4 (0 : map (+1) (0 : map (+1) (0 : map (+1) nats)))
take 4 (0 : map (+1) (0 : map (+1) (0 : map (+1) (0 : map (+1) nats))))
take 4 (0 : 0+1 : map (+1) (0 + 1 : map (+1) (0 + 1 : map (+1) nats))))
take 4 (0 : 1 : (0+1+1) : map (+1) (0 + 1 + 1 : map (+1) nats))
take 4 (0 : 1 : 2 : 0 + 1 + 1 + 1 : map (+1) nats)
take 4 ([0,1,2,3] : map (+1) nats)
[0,1,2,3]
-}

-- TODO: todo el tema del profiling y demás