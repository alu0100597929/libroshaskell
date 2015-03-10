sucesor :: Integer -> Integer
sucesor x = x + 1

sumaCuadrados :: Integer -> Integer -> Integer
sumaCuadrados x y = x*x + y*y

infinito :: Integer
infinito = 1 + infinito

cero :: Integer -> Integer
cero x = 0

maximo :: Integer -> Integer -> Integer
maximo x y = div ((x + y) + abs (x - y)) 2

entre0y9 :: Integer -> Bool
entre0y9 x = (x >= 0) && (x <= 9)

predSuc :: Integer -> (Integer, Integer)
predSuc x = (x - 1, x + 1)

inc :: Integer -> Integer
inc x = x + 1

esCero :: Integer -> Bool
esCero x = (x == 0)

componer :: (Integer -> Integer) -> (Integer -> Integer) -> Integer -> Integer
componer f g x = f (g x)

--cosa nueva, definimos el operador "proporcional", no asociativo y con
--prioridad 4
infix 4 ~=
(~=) :: Float -> Float -> Bool
x ~= y = abs (x - y) < 0.0001

f :: Integer -> Bool
f 1 = True
f 2 = False
f x = True

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

suma :: [Integer] -> Integer
suma [] = 0
suma (x:xs) = x + suma xs

aEntero :: [Int] -> Int
aEntero [d] = d
aEntero (d:m:r) = aEntero ((10 * d + m):r)

primero2 :: (Integer, Integer) -> Integer
primero2 (x, y) = x

primero3 :: (Integer, Integer, Integer) -> Integer
primero3 (x, y, z) = x

sumaPares :: [(Integer, Integer)] -> Integer
sumaPares [] = 0
sumaPares ((x,y):xs) = x + y + sumaPares xs

--los patrones n+k no están soportados a partir de Haskell 2010
{-
factorial :: Integer -> Integer
factorial 0 = 1
factorial (n + 1) = (n + 1) * factorial n
-}

--ver página 35, patrones nombrados o pseudónimos

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

{-Cuidado, no se pueden repetir nombres de variables en la parte izquda.
la excepción es el patrón subrayado '_' como se ve en la función
siempreVerdad-}
sonIguales :: Integer -> Integer -> Bool
sonIguales x y = (x == y)

siempreVerdad :: Integer -> Integer -> Bool
siempreVerdad _ _ = True

esVacia :: [a] -> Bool
esVacia [] = True
esVacia (_:_) = False

--definimos una lista infinita
infinita :: [Integer]
infinita = 1 : infinita

--longitud de una lista con una expresión case. Mirar atentamente la
--indentación de estas expresiones
long :: [Integer] -> Integer
long ls = case ls of
            []      -> 0
      	    _:xs    -> 1 + long xs

cabeza :: [a] -> a
cabeza [] = error "las listas vacías no tienen cabeza"
cabeza (x:_) = x

{-funciones a trozos: guardas, indentación obligatoria, expresiones
booleanas, de arriba a abajo-}
absoluto :: Integer -> Integer
absoluto n
    | n >= 0    = n
    | otherwise = -n

signo :: Integer -> Integer
signo n
    | n > 0     = 1
    | n == 0    = 0
    | n < 0     = -1

maxEnt :: Integer -> Integer -> Integer
maxEnt x y = if x >= y then x else y

--truco de los if, usar la expresión como operando:
--2 * if 'a' < 'z' then 10 else 4. RESULTADO = 20

{-Cláusulas where --> encapsulamiento básico en Haskell, los módulos son
mejores-}
--función que calcula las raíces de una ecuación de segundo grado si los
--coeficientes son reales:
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c
    | disc >= 0 = ((-b + raizDisc) / denom, (-b - raizDisc) / denom)
    | otherwise = error "raíces complejas"
    where
	disc = b^2 - 4*a*c
	raizDisc = sqrt disc
	denom = 2 * a

--Recuerda: los where son locales a la función, no pueden usarse fuera
--además, deben tener la indentación bien hecha. Los where no necesarios
--para calcular el resultado serán ignorados por Haskell

{-Se hacen dos divisiones entre 60 sucesivas y nos quedamos con el
primer resto (segundos), el segundo resto (minutos) y el segundo
resultado (horas)-}
descomponer :: Integer -> (Integer, Integer, Integer)
descomponer secs = (segunda_div, mod_segunda_div, segundos)
    where
		segundos = secs `mod` 60 --segundos
		primera_div = secs `div` 60
		segunda_div = primera_div `div` 60 --horas
		mod_segunda_div = primera_div `mod` 60 --minutos

aLaDerechaDe :: Integer -> Integer -> Integer
aLaDerechaDe x y = y*10 + x

--usando restas
restoDivEntera :: Integer -> Integer -> Integer
restoDivEntera x y
  | (x-y) >= 0 = restoDivEntera (x-y) y
  | otherwise  = x

--usando sumas y restas
cocienteDivEntera' :: Integer -> Integer -> Integer -> Integer
cocienteDivEntera' x y c
  | (x-y) >= 0 = cocienteDivEntera' (x-y) y (c+1)
  | otherwise  = c

cocienteDivEntera :: Integer -> Integer -> Integer
cocienteDivEntera x y = cocienteDivEntera' x y 0

sumDesdeHasta :: Integer -> Integer -> Integer
sumDesdeHasta a b
  | a < b = a + sumDesdeHasta (a+1) b
  | otherwise = b

mulDesdeHasta :: Integer -> Integer -> Integer
mulDesdeHasta a b
  | a < b = a * mulDesdeHasta (a+1) b
  | otherwise = b

variaciones :: Integer -> Integer -> Integer
variaciones m n = fact m `div` fact (m-n)

variaciones' :: Integer -> Integer -> Integer
variaciones' m n = mulDesdeHasta (m-n+1) m

combinatorio :: Integer -> Integer -> Integer
combinatorio m n = fact m `div` (fact (m-n) * fact n)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--mejorable
mayorDeTres :: Integer -> Integer -> Integer -> Integer
mayorDeTres a b c
  | a >= b && a >= c = a
  | b > c = b
  | otherwise = c

--mejorable
mayorDeCuatro :: Integer -> Integer -> Integer -> Integer -> Integer
mayorDeCuatro a b c d
  | a >= b && a>= c && a>= d = a
  | b >= c && b >= d = b
  | c >= d = c
  | otherwise = d

--por hacer
{-ternaOrdenada :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
ternaOrdenada a b c = (mayor, medio, menor)
  where
    mayor = mayorDeTres(a,b,c)
    medio =
    menor =
-}

--solo vale para números de 4 cifras
esCapicua :: Integer -> Bool
esCapicua a
  | a < 1000 || a > 9999 = error "número de cifras incorrecto"
  | a `mod` 10 == (a `div` 1000) && (a `div` 10) `mod` 10 == (a `div` 100) `mod` 10 = True
  | otherwise = False

sumaCifras :: Integer -> Integer
sumaCifras 0 = 0
sumaCifras a = a `mod` 10 + sumaCifras (a `div` 10)

--mejorable
numCifras :: Integer -> Integer
numCifras 0 = 0
numCifras n = 1 + numCifras (n `div` 10)

trocear :: Integer -> (Integer, Integer)
trocear n = (n `div` 10, n `mod` 10)

concatenar :: Integer -> Integer -> Integer
concatenar a b = a * 10 ^ (numCifras b) + b

{-

-}