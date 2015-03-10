--ejemplo de funciones curry

--se suele evitar poner las definiciones (cabeceras) de las funciones
--para evitar errores de ejecución

--versión curry de la función add, para crear funciones a partir de ella
--sólo uso (sustituyo por un valor) el primer parámetro y luego el otro
--viene en la llamada a la función creada
add' :: Int -> (Int -> Int)
add' x y = x + y

--función hecha por mí, declaración de tipos mejorable
--ya tiene un parámetro de add' por tanto sólo necesita otro
add1 :: Int -> Int
add1 = add' 1

--para crear funciones a partir de esta, sustituyo primero x, luego y,
--luego z (o solo x, o solo x e y)
multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x*y*z

--aquí solo sustituyo x, por tanto a multTwoWithNine hay que pasarle 2
--parámetros que le faltan al llamarla
multTwoWithNine = multThree 9

--función hecha por mí, declaración de tipos mejorable
--multiplica x por el cuadrado de y
multiplicaporcuadrado :: Int -> Int -> Int
multiplicaporcuadrado x y = multThree x y y 

--ejemplo de parámetro funcion, recibe una tupla
curry :: ((a,b)->c) -> a->b->c
curry f a b = f (a,b)

--ejemplo de parámetro función (se necesita trabajar esto...)
--no se si es un ejemplo de función curry
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--Ejemplo página oficial de Haskell, se evita las definiciones de las
--funciones para evitar errores 

--a esto sólo le tengo que pasar n, la xs es parámetro de las funciones
--creadas a partir de ésta
multList n [] = []
multList n (x:xs) = n*x : multList n xs

--curioso: no sale xs por ningún lado, aunque se la debemos pasar
listapor2 = multList 2 --ya tiene el primer parámetro, falta la lista
listapor3 = multList 3

--But now, if we had the function

addToList n [] = []
addToList n (x:xs) = n+x : addToList n xs

--we could parameterize the difference again

--para crear funciones a partir de esta, debemos pasar un numero y un
--operador (bop = Binary OPerator)
operList n bop [] = []
operList n bop (x:xs) = bop n x : operList n bop xs
--es válido hacer (*) 2 3, pero tienen que estar los paréntesis

{-
ghci> (*) 2 3
6
ghci> (2*) 8
16
-}

--and define doubleList as

doubleList = operList 2 (*)

--but this ties us into a constant parameters
--and we could redefine things as

--una vez más, para crear funciones sólo les tengo que pasar la f
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs
--and define doubleList as

doubleList' = mapList (2*)

--mapList equivale a la función map de Haskell
