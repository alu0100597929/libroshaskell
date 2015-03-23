import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

{-
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)
-}

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  |                    otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  |                    otherwise = Nothing

{-
ejemplo erróneo, debería dar Nothing. Corregido más abajo usando (>>=)
ghci> (0, 0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
(0,2)

ghci> landRight 1 (0, 0) >>= landLeft 2
Just (2,1)

ghci> Nothing >>= landLeft 2
Nothing

ghci> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
Just (2,4)

ghci> return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
Just (2,4)

--corrección de la interactuación de landLeft/landRight para propagar el error.
ghci> return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
Nothing
-}

banana :: Pole -> Maybe Pole
banana _ = Nothing

{-
ghci> return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1
Nothing
-}

{-
(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
-}

{-
ghci> Nothing >> Just 3
Nothing

ghci> Just 3 >> Just 4
Just 4

ghci> Just 3 >> Nothing
Nothing

ghci> return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1
Nothing
-}

-- esto sería una secuencia si no usáramos >> ni >>=
routine :: Maybe Pole
routine = case landLeft 1 (0, 0) of
  Nothing -> Nothing
  Just pole1 -> case landRight 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft 1 pole3

{-
ghci> Just 3 >>= (\x -> Just (show x ++ "!"))
Just "3!"

ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Just "3!"

ghci> let x = 3; y = "!" in show x ++ y
"3!"

ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
Nothing
ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
Nothing
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))
Nothing
-}

foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

{-
ghci> Just 9 >>= (\x -> Just (x > 8))
Just True

marySue :: Maybe Bool
marySue = do
x <- Just 9
Just (x > 8)
-}

routine' :: Maybe Pole
routine' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

routine'' :: Maybe Pole
routine'' =
  case Just (0, 0) of
    Nothing -> Nothing
    Just start -> case landLeft 2 start of
      Nothing -> Nothing
      Just first -> case landRight 2 first of
        Nothing -> Nothing
        Just second -> landLeft 1 second

{-En este ejemplo se ve un poco más clara la notación do, cuando aplicamos landLeft
o landRight tenemos que especificar sobre qué Pole vamos a trabajar, aunque SIEMPRE
se usará el resultado de la pole del último landX hecho (aunque sea devolver directa-
mente un Nothing.

(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n

Si no bindeamos a un nombre, do entiende que estamos usando >> en vez de >>=
-}
routine''' :: Maybe Pole
routine''' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing                    -- \_ <- Nothing
  second <- landRight 2 first
  landLeft 1 second

{-
Nothing >>= loquesea dará siempre Nothing

return inyecta un valor en una mónada (contenedor).

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing

landLeft 2 >> Nothing ignora el primer parámetro y devuelve el segundo (Nothing).

Nothing >> Just 3 es algo más complicado, veamos la definición de (>>):

(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n

y recordamos que "Nothing >>= loquesea dará siempre Nothing", por tanto esto devuelve Nothing.

Just 3 >> Just 4 hace que m se enlace a Just 3 y n se enlace a Just 4, daría:

Just 3 >>= Just 4 = Just 3 >>= \_ -> Just 4

Cuya parte derecha, Just 3 >>= \_ -> Just 4, viendo la definición, da:

(\_ -> Just 4) Just 3, lo cual se ve que da Just 4

De los siguientes ejemplos podemos deducir que lo que hace >> es decir: "ignora el valor del parám. izq.
de >>, ahora la computación tiene un valor de (parám. der.)".

*Main> return (0,0) >>= landLeft 2 >> return (4,4) >>= landRight 2 >>= landLeft 1
Just (5,6)

La función routine''' con la notación de >>= y >>
*Main> return (0,0) >>= landLeft 2 >> Nothing >>= landRight 2 >>= landLeft 1
Nothing

En resumen, << lo que hace es:

Ignora el parámetro izquierdo y devuelve el derecho, a no ser que el parámetro izquierdo tenga un
valor que <<= no pueda extraer:

*Main> Nothing >> Just 9
Nothing
-}

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello" -- cojo lo que hay dentro de la mónada y reconozco el patrón
  return x -- envuelvo la cabeza de la lista en un contexto monádico de Maybe Char

-- fail :: (Monad m) => String -> m a
-- fail msg = error msg

fail :: t -> Maybe a
fail _ = Nothing

--Cuando el reconocimiento de patrones falla, se llama a fail, que tal y como la hemos
-- definido produce un Nothing
wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

{-
ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-}

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1,2]
  ch <- ['a','b']
  return (n, ch)

{-
ghci> [ (n, ch) | n <- [1,2], ch <- ['a','b'] ]
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-}

-- MonadPlus: la clase de tipos para mónadas que pueden actuar como monoides
{-
ghci> [ x | x <- [1..50], '7' `elem` show x ]
[7,17,27,37,47]

ghci> guard (5 > 2) :: Maybe ()
Just ()
ghci> guard (1 > 2) :: Maybe ()
Nothing
ghci> guard (5 > 2) :: [()]
[()]
ghci> guard (1 > 2) :: [()]
[]

ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
[7,17,27,37,47]

ghci> guard (5 > 2) >> return "cool" :: [String]
["cool"]
ghci> guard (1 > 2) >> return "cool" :: [String]
[]

Es decir, guard lo que hace es; si la computación tiene éxito, devuelve un valor
"tonto" (). Si falla, devuelve mzero, es decir, el elemento neutro, para no
fastidiar la computación.

Después de guard se pone siempre >> para ignorar lo que devuelve y quedarnos con
el valor que nos interesa.

Si guard falla, también lo hará el return que vaya después, porque alimentar >>=
con la lista vacía siempre devolverá una lista vacía.

"Si el Booleano es False, produce un fallo; si no, produce un valor tonto () para
que la computación pueda continuar"
-}

filtrar = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

{-
ghci> [ x | x <- [1..50], '7' `elem` show x ]
[7,17,27,37,47]
-}

--1: columnas 2: filas (al revés que normalmente)
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c, r) = filter onBoard
  [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
  ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
  ]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- TODO ejercicio: hacer una función tal que, si se puede llegar a ese escaque,
-- nos diga cómo devolviendo la lista de movimientos

--comoLLegar :: KnightPos -> KnightPos -> Maybe [KnightPos]
--comoLLegar start end = case canReachIn3 start end of False -> Nothing
--                                                     True  -> 

-- composición normal
{-
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = (\x -> f (g x))
-}

-- composición con mónadas
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)

{-
ghci> let f x = [x,-x]
ghci> let g x = [x*3,x*2]
ghci> let h = f <=< g
ghci> h 3
[9,-9,6,-6]
-}