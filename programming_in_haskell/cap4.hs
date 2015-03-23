{- así está en el libro
isDigit :: Char -> Bool
isDigit d = (d >= '0') && (d <= '9')
-}

isDigit :: Char -> Bool
isDigit d = d `elem` ['0'..'9']

isEven :: Integral a => a -> Bool
isEven x = x `mod` 2 == 0 

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1 / n

--importante el uso de class constraints en isEven y recip' para darnos
--cuenta de que se pueden aplicar a los tipos integrales y fraccionales

valorabsoluto :: Int -> Int
valorabsoluto n = if n < 0 then (-n) else n

--cuidado, cuando ponemos las definiciones mal el programa no compila

{-
signum' :: Int -> Int
signum' n = if n < 0 then (-1) else
              if n == 0 then 0 else 1
-}

--Ejemplos de uso de guardianes, cada uno de los cuales se lee como:
--"de tal forma que"
abs' n | n >= 0 = n
       | otherwise = (-n)
	      
signum' n | n > 0 = 1
          | n == 0 = 0
          | otherwise = -1

{- Código del Prelude	  
True  && x = x
False && _ = False
-}

--Coincidencia de patrones (patter matching)
--recuerda: '_' es el carácter comodín, que casa con todo y se usa cuan-
--do no nos interesa tener lo que casa en una variable

fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b --si no ponemos que es b, está mal
snd' (_,x) = x

lEmpiezaPorA :: [Char] -> Bool
lEmpiezaPorA ['a',_,_] = True
lEmpiezaPorA _ = False --cualquier otro tipo de lista

lEmpiezaPorA' :: [Char] -> Bool
lEmpiezaPorA' ('a':_) = True
lEmpiezaPorA' _ = False 

null' :: [a] -> Bool
null' [] = True
null' _ = False

--(x:xs) y todos estos se parentizan porque sin paréntesis la aplicación
--de funciones tiene mayor prioridad

head' :: [a] -> a
head' [] = error "No se puede aplicar head' a una lista vacía"
head' (x:_) = x

--tail quita el primer elemento de una lista no vacía
tail' :: [a] -> [a]
tail' [] = error "No se puede aplicar tail' a una lista vacía"
tail' (_:xs) = xs

--patrones de enteros, son de la forma (n+k) donde n es variable y k es
--constante > 0
pred' :: Int -> Int
pred' 0 = 0
pred' n = n -1

{-Lambdas, son funciones sin nombre, la lambda se escribe '\'-}

-- \x -> x + x significa que lo que está a la izq. de la flecha son los
--parámetros, y a la derecha la forma en la que se calcula la función

--esta nos sirve para entender mejor las funciones "curried"
--recibe un número x y devuelve una función que recibe y y suma el valor
--de y con el de x
add = \x -> (\y -> x + y)

suma1 = add 1

{-
const' :: a -> b -> a
const' x _ = x
-}

--devolvemos una función (por eso está entre paréntesis)
const' :: a -> (b -> a)
const' x = \_ -> x

{-expliación de lo anterior

You seem to be thinking that this is equivalent to const (id 6) 5, where
id 6 evaluates to 6, but it isn't. Without those parentheses, you're 
passing the id function as the first argument to const. So look at the 
definition of const again:

const x _ = x
This means that const id 6 = id. Therefore, const id 6 5 is equivalent 
to id 5, which is indeed 5.-}

{-
odds :: Int -> [Int]
odds n = [1,3..n]
-}

{-
odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x * 2 + 1
-}
	 
--es decir, funciones que sólo se usan una vez se pueden expresar como
--lambdas	 
odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]

--ver el capítulo 4.6 de este libro (secciones), es muy bueno

{-Ejercicios cap 4-}

--sólo funciona para listas de tamaño par, con impares hace lo que puede
--y bastante bien
halve' :: [a] -> ([a],[a])
halve' xs = (take mitad xs, drop mitad xs)
            where mitad = (length xs) `div` 2

--funciona como tail pero hace un append de la lista vacía también

safetailc :: [a] -> [a]
safetailc xs = if null xs then [] else tail xs    

--(x:xs) nunca va a casar con la lista vacía

safetailg :: [a] -> [a]
safetailg xs | null xs = []
             | otherwise = tail xs

safetailp :: [a] ->[a]
safetailp [] = []
safetailp (x:xs) = xs

--operador disyunción v (or)
disj :: Bool -> Bool -> Bool
disj False False = False
disj _ _ = True

--operador conjunción ^ (and) mediante expresiones condicionales
--es más largo porque no quise usar && pues sería "hacer trampa"
conj :: Bool -> Bool -> Bool
conj x y = if x == True then
              if y == True then True else False
	   else False
	   
--hacemos lo mismo para esta versión
{-
True ∧ b = b
False _ ∧ = False
-}

conj' :: Bool -> Bool -> Bool
conj' x y = if x == True then y else False

--poner mult x y z = x * y * z en función de expresones lambda
--ejemplo: add = \x -> (\y -> x + y)

mult = \x -> (\y -> (\z -> x * y * z))

multiplicapor5 = mult 5
--multiplicapor5 6 7 daría 210 pues la x se "fijó" en 5
multiplicapor5y7 = multiplicapor5 7
multiplicapor5y7y9 = multiplicapor5y7 9
