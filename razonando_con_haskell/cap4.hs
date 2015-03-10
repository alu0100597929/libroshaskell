--Capítulo 4, definición de tipos
import Data.Char --ord, chr
import Data.List

type Entero = Integer

type DeEnteroEnEntero = Entero -> Entero

tres :: Entero
tres = 3

sucesor :: DeEnteroEnEntero
sucesor = (+) 1

type ParFlotantes = (Float, Float)

parCeros :: ParFlotantes
parCeros = (0.0, 0.0)

--nombre :: String
--nombre = ['p','e','p','e']

nombreYApellidos :: String
nombreYApellidos = "José E. Gallardo"

--definición de tipos de datos

--tipos enumerados

--Diasemana es un constructor de tipos (nombre del nuevo tipo definido)
--mientras que Lunes, Martes...son constructores de datos escritos en forma literal
--los dos constructores empiezan por mayúscula. No pueden definirse dos constructores idénticos en el mismo programa
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

unDia :: DiaSemana
unDia = Miercoles

laborables :: [DiaSemana]
laborables = [Lunes, Martes, Miercoles, Jueves, Viernes]

esFinSemana :: DiaSemana -> Bool
esFinSemana Sabado = True
esFinSemana Domingo = True
esFinSemana _ = False

data Direccion = Norte | Sur | Este | Oeste deriving Show

girar90 :: Direccion -> Direccion
girar90 Norte = Este
girar90 Este = Sur
girar90 Sur = Oeste
girar90 Oeste = Norte

data ColorSemaforo = Rojo | Verde | Amarillo deriving Show

{-Ejercicio 4.1-}

--0 significa domingo, 1 lunes...etc
diaSemana' :: Int -> Int -> Int -> DiaSemana
diaSemana' d m a = dias !! ((700 + (26*x -2) `div` 10 + d + y + y `div` 4 + z `div` 4 - 2*z) `mod` 7)
                    where
                      x = if (m <= 2) then
                                          m + 10
                                      else
                                          (m - 2)
                      y = if (m <= 2) then
                                          (a-1) `mod` 100
                                      else
                                          a `mod` 100
                      z = if (m <= 2) then
                                          (a-1) `div` 100
                                      else
                                          a `div` 100
                      dias = [Domingo] ++ laborables ++ [Sabado]

data LetraOEntero = Letra Char | Entero Integer deriving Show

unValor :: LetraOEntero
unValor = Letra 'x'

otroValor :: LetraOEntero
otroValor = Entero 15

listaMixta :: [LetraOEntero]
listaMixta = [Letra 'a', Entero 10, Entero 12, Letra 'b']

--los constructores de datos pueden actuar como funciones

incLoE :: LetraOEntero -> LetraOEntero
incLoE (Entero n) = Entero . (+1) $ n
incLoE (Letra c) = Letra . chr . (+1) . ord $ c

-- *Main> incLoE $ Letra 'b'
-- Letra 'c'

data Temp = Centigrado Float | Farenheit Float deriving Show

estaCongelado :: Temp -> Bool
estaCongelado (Centigrado c) = c <= 0.0
estaCongelado (Farenheit f)  = f <= 32.0

--significa Par :: Integer -> Integer -> Racional
data Racional = Par Integer Integer deriving Show

unMedio :: Racional
unMedio = Par 1 2

numerador :: Racional -> Integer
numerador (Par x _) = x

denominador :: Racional -> Integer
denominador (Par _ x) = x

por :: Racional -> Racional -> Racional
(Par a b) `por` (Par c d) = Par (a*c) (b*d)

--los constructores de datos tienen dos funciones, si está en la parte derecha actúa como función
--construyendo un nuevo dato a partir de sus componentes
unRacional :: Racional
unRacional = Par 3 7

--Si aparece en la parte izquierda, actúa como patrón, y permite descomponer un dato en sus componentes
--ver funciones numerador y denominador

type Nombre = String
type Apellido = String
type Edad = Integer
data Persona = UnaPersona { nombre :: Nombre
                          , apellido1 :: Apellido
                          , apellido2 :: Apellido
                          , edad :: Edad
                          } deriving Show

juan :: Persona
juan = UnaPersona { nombre = "Juan"
                  , apellido1 = "Yoqse"
                  , apellido2 = "Tio"
                  , edad = 24
                  }

--esto automatiza la creación de funciones selectoras de sus componentes, de modo que puedo
--ejecutar (edad juan)

--Se puede construir un nuevo valor a partir de otro existente, indicando el valor de las componentes
--que difieren:

-- *Main> juan {apellido1 = "Riera", apellido2 = "Salazar"}
-- UnaPersona {nombre = "Juan", apellido1 = "Riera", apellido2 = "Salazar", edad = 24}

--como Haskell es un lenguaje funcional, el valor original de juan no se modifica

cumpleanios :: Persona -> Persona
cumpleanios p = p {edad = edad p + 1}

--el operador @ (As-pattern)
cumpleanios' :: Persona -> Persona
cumpleanios' p@UnaPersona{edad = e} = p {edad = e + 1}

--los constructores de tipos y de datos pueden coincidir en nombre

data Valido = Valido Integer deriving Show

unValorValido :: Valido
unValorValido = Valido 24

--constructores con variantes, este tiene 4 variantes, cada una con su aridad:

type Radio = Float
type Lado = Float

data Figura = Circulo Radio
            | Cuadrado Lado
            | Rectangulo Lado Lado
            | Punto
              deriving Show

unCirculo :: Figura
unCirculo = Circulo 25.0

unRectangulo :: Figura
unRectangulo = Rectangulo 10.0 20.0

listaFiguras :: [Figura]
listaFiguras = [Circulo 25.0, Cuadrado 10.0, Rectangulo 20.0 15.0]

area :: Figura -> Float
area (Circulo r) = pi * (r)^2
area (Cuadrado l) = l^2
area (Rectangulo l1 l2) = l1 * l2
area (Punto) = error "los puntos no tienen área medible"

perimetro :: Figura -> Float
perimetro (Circulo r) = 2 * pi * r
perimetro (Cuadrado l) = l*4
perimetro (Rectangulo l1 l2) = l1*2 + l2*2
perimetro (Punto) = error "los puntos no tienen perímetro medible"

data Complejo = Float :- Float deriving Show

origen :: Complejo
origen = 0.0 :- 0.0

parteReal :: Complejo -> Float
parteReal (r :- _) = r

data Resultado = UnaReal Float
               | DosReales Float Float
               | DosComplejas Complejo Complejo
                 deriving Show

raices :: Float -> Float -> Float -> Resultado
raices a b c
  | discriminante < 0 = DosComplejas (0.0 :- 0.0) (0.0 :- 0.0) --falta completar
  | discriminante == 0 = UnaReal $ -b / (2*a)
  | otherwise = DosReales ((-b + sqrt discriminante) / (2*a)) ((-b - sqrt discriminante) / (2*a))
  where
    discriminante = b^2 - 4*a*c

--hasta ahora, hemos definido tipos (conjuntos de valores) enumerados, finitos. Para definir tipos infinitos
--debemos usar inducción matemática.

data Nat = Cero | Suc Nat deriving Show --representa los números naturales, en plan primitiva recursiva

uno :: Nat
uno = Suc Cero

dos :: Nat
dos = Suc (Suc Cero)

indefinidoN :: Nat
indefinidoN = undefined

infinitoN :: Nat
infinitoN = Suc infinitoN

esPar :: Nat -> Bool
esPar Cero = True
esPar (Suc x) = not (esPar x)

--creamos el operador <+> de modo recursivo sobre su segundo argumento:

{-infixl 6 <+>
(<+>) :: Nat -> Nat -> Nat
m <+> Cero = m
m <+> (Suc n) = Suc (m <+> n)-}

--ahora lo mismo pero con su primer argumento, no siempre se puede hacer la definición sobre los dos
--argumentos, y a veces elegir mal puede complicarlo demasiado o incluso hacerlo imposible

infixl 6 <+>
(<+>) :: Nat -> Nat -> Nat
Cero <+> n = n
(Suc m) <+> n = Suc (m <+> n)

--m * (n+1) = m*n + m
{-infixl 7 <*>
(<*>) :: Nat -> Nat -> Nat
m <*> Cero = Cero
m <*> (Suc n) = m <*> n <+> m-}

--m^(n+1) = m ^ n * m 
{-infixr 8 <^>
(<^>) :: Nat -> Nat -> Nat
m <^> Cero = Suc Cero
m <^> (Suc n) = m <^> n <*> m-}

pred' :: Nat -> Nat --necesario para el operador resta
pred' Cero = Cero
pred' (Suc n) = n

--m - (n+1) = (m - n) - 1 por tanto es el predecesor de (m - n)
infixl 6 <->
(<->) :: Nat -> Nat -> Nat
m <-> Cero = m
m <-> (Suc n) = pred' $ m <-> n

positivo :: Nat -> Bool
positivo Cero = False
positivo (Suc n) = True 

mayor :: Nat -> Nat -> Bool
mayor m n = positivo (m <-> n)

menor :: Nat -> Nat -> Bool
menor m n = not $ mayor m n
--se necesita un acumulador, que empiece en 0

igual :: Nat -> Nat -> Bool
igual m n = (not (mayor m n)) && (not (menor m n)) 

{-
--TODO
divNat m n = divNat' n m 0

divNat' :: Nat -> Nat -> Nat -> Nat
divNat' m 1 acc = m
divNat' m (Suc n) acc = m <-> n
-}
--TODO

--ejercicio 4.7

{-factorial :: Nat -> Nat
factorial Cero = uno
factorial (Suc Cero) = uno
factorial n = n <*> factorial (pred' n)-}

{-
--TODO
infixl 7 <*>
(<*>) :: Nat -> Nat -> Nat
Cero <*> m = Cero
(Suc n) <*> m = n <*> m <+> m-}

--binomial :: Nat -> Nat -> Nat
--binomial n k = (factorial n) / (factorial k * factorial (n-k))

-----------------------------------------------------------------

data Expr = Valor Integer
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
            deriving Show

ej1 :: Expr
ej1 = Valor 5

ej2 :: Expr
ej2 = ej1 :+: Valor 3

ej3 :: Expr
ej3 = ej2 :*: Valor 10

indefinidoE :: Expr
indefinidoE = undefined

numOpers :: Expr -> Integer
numOpers (Valor _) = 0
numOpers (e1 :+: e2) = numOpers e1 + 1 + numOpers e2
numOpers (e1 :-: e2) = numOpers e1 + 1 + numOpers e2
numOpers (e1 :*: e2) = numOpers e1 + 1 + numOpers e2

--ejercicio 4.9

valorDe :: Expr -> Integer
valorDe (Valor x) = x
valorDe (e1 :+: e2) = valorDe e1 + valorDe e2
valorDe (e1 :-: e2) = valorDe e1 - valorDe e2
valorDe (e1 :*: e2) = valorDe e1 * valorDe e2

--ejercicio 4.10
numConsts :: Expr -> Integer
numConsts (Valor _) = 1
numConsts (e1 :+: e2) = numConsts e1 + numConsts e2
numConsts (e1 :-: e2) = numConsts e1 + numConsts e2
numConsts (e1 :*: e2) = numConsts e1 + numConsts e2

--ejercicio 4.11
anidMax :: Expr -> Integer
anidMax (Valor _) = 0
anidMax (e1 :+: e2) = anidMax e1 + 1 + anidMax e2
anidMax (e1 :-: e2) = anidMax e1 + 1 + anidMax e2
anidMax (e1 :*: e2) = anidMax e1 + 1 + anidMax e2

--funciones de plegado, catamorfismos o recursores

foldNat :: (a -> a) -> a -> Nat -> a
foldNat f e Cero = e
foldNat f e (Suc n) = f (foldNat f e n)

esPar' :: Nat -> Bool
esPar' = foldNat not True

--ejercicio 4.12
--pasamos la multiplicacion de enteros a sumar n veces m
{-infixl 7 <*>
(<*>) :: Nat -> Nat -> Nat
(<*>) m = foldNat (<+> m) Cero-}

--plegado de expresiones
foldExpr :: (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) ->
            (Integer -> a) -> (Expr -> a)
foldExpr f g h j = fun
  where
    fun (Valor n) = j n
    fun (e1 :+: e2) = (fun e1) `f` (fun e2)
    fun (e1 :-: e2) = (fun e1) `g` (fun e2)
    fun (e1 :*: e2) = (fun e1) `h` (fun e2)

valorDe' = foldExpr (+) (-) (*) id

--en el libro hablan de s y z como lambdas, aunque aquí (ejemplo del libro)
--se les da nombre
numOpers' :: Expr -> Integer
numOpers' = foldExpr s s s z where { s x y = x + 1 + y; z x = 0 }

--ejercicio 4.13
numConsts' :: Expr -> Integer
numConsts' = foldExpr (+) (+) (+) z where { z x = 1 }

--ejercicio 4.14
anidMax' :: Expr -> Integer
anidMax' = foldExpr s s s z where { s e1 e2 = e1 + 1 + e2 ;z x = 0 }

--tipos polimórficos
data Par a = UnPar a a deriving Show

--permite construir listas mixtas, y también homogéneas
{-
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

listaMixta' :: [Either Char Integer]
listaMixta = [Left 'a', Right 3]-}

--Maybe es un tipo que se usa como resultado de funciones parcialmente definidas
reciproco :: Float -> Maybe Float
reciproco 0 = Nothing
reciproco x = Just (1/x)

--ejercicio 4.15
--type Nombre = String  *******definido más arriba*******
type Telefono = Integer
type Agenda = [(Nombre, Telefono)]

unaAgenda :: Agenda
unaAgenda = [("Freinn", 637561754),
             ("Morsi", 660898252),
             ("Sergio", 609419076)]

buscar :: Agenda -> Nombre -> Maybe Telefono
buscar [] nom = Nothing
buscar ((x,y):xs) nom = if (x == nom) then
                                        Just y --importante el Just, si no, error sano
                                      else
                                        buscar xs nom

newtype Natural = UnNatural Integer deriving Show

aNatural :: Integer -> Natural
aNatural x
  | x < 0 = error "No es posible usar enteros negativos"
  | otherwise = UnNatural x

desdeNatural :: Natural -> Integer
desdeNatural (UnNatural x) = x

--TODO pág. 90, diferencias entre las declaraciones data y newtype

espejo :: Expr -> Expr
espejo (Valor n) = Valor n
espejo (e1 :+: e2) = espejo e2 :+: espejo e1
espejo (e1 :-: e2) = espejo e2 :-: espejo e1
espejo (e1 :*: e2) = espejo e2 :*: espejo e1