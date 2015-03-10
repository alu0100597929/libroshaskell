module Shapes
( Point(..) --exporta todos los constructores de valor del tipo Point
, Shape(..) --exporta todos los constructores de valor del tipo Shape
, area      --si quitáramos los paréntesis de los dos tipos de arriba, no podrían usarse constructores de valor
, nudge     --por tanto, sólo podrían crear Shapes con las funciones baseCircle y baseRect
, baseCircle
, baseRect
) where

data Bool = False | True

data Point = Point Float Float deriving (Show)

--los constructores valor como Circle y Rectangle son funciones como otra cualquiera, se le pasan los datos y
--devuelve valores de un tipo (en este caso Shape)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
--show convierte a cadena cualquier cosa, Haskell siempre llama al método show
--antes de mostrar cualquier cosa por pantalla

--el tipo es Shape (forma), un Circle (círculo) es un constructor de Shape, no un tipo
area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-
ghci> Circle 10 20 5
Circle 10.0 20.0 5.0
ghci> Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
-}

{-
--los constructores de valor son funciones, por lo cual podemos mapearlos, aplicarlos parcialmente, etc.
ghci> map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0
20.0 6.0]
-}

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

{-
ghci> nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
-}

--data Person = Person String String Int Float String String deriving (Show)

{-
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-}

{-
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
-}

--Record syntax. Lo mejor es que cuando derivamos Show, todos los campos se especifican con su nombre,
--y serán fácilmente reconocibles, sobre todo cuando no son obvios.

{-
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
-}

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

{-
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
-}

--constructores de tipos (tienen que tener rellenos todos los parámetros de tipo)

--no existe un tipo Maybe, sino Maybe Int, Maybe Char...etc.
data Maybe a = Nothing | Just a

--Las listas también usan parámetros de tipo, no existe el tipo [] sino [Int], [Char], [[(String,Int)]]...etc.

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-Coche parametrizado
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) =
"This " ++ c ++ " " ++ m ++ " was made in " ++ show y

Esto no tiene demasiado sentido ya que el único beneficio sería poner al parámetro c el tipo que queramos
(mientras sea instancia de Show), mientras que la declaración de tipos se complica.        
-}

data Vector a = Vector a a a deriving (Show) --como se ve, todas las componentes del vector tienen que tener
--el mismo tipo
--importante, el tipo es lo de antes del igual "Vector a", porque a pesar de que su constructor de valor
--"Vector a a a" recibe tres valores, el constructor de tipo "Vector a" sólo recibe uno.

--las 3 operaciones siguientes necesitan dos vectores del mismo tipo (y todos de la clase de tipos Num)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

--para que un tipo pueda derivar de Eq, todos los "campos" deben ser parte de la clase de tipos Eq.
--para que una instancia de este tipo sea igual a otra, todos los "campos" deben coincidir
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

--No se hubiera podido imprimir por pantalla una Person si no se deriva a Show.

{-
*Recuerda:*, la función show pasa cualquier tipo derivado de Show a una String

ghci> mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
ghci> "mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
-}

mysteryDude = "Person { firstName =\"Michael\"" ++
                      ", lastName =\"Diamond\"" ++
                      ", age = 43}"

{-
--si queremos leer esta string, debemos informar a Haskell de qué tipo queremos:
ghci> read mysteryDude :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}

--si Haskell es capaz de inferir el tipo, por ejemplo por compararlo con un valor del mismo tipo, no hace falta
--anotar el tipo
ghci> read mysteryDude == mikeD
True

--esto da un error:
ghci> read "Just 3" :: Maybe a

--esto funciona bien:
ghci> read "Just 3" :: Maybe Int
Just 3
-}

--Enum: elementos que se pueden enumerar, como los días de la semana, antes de x va y, después de x va z, etc.
--podemos usar: succ, pred, y crear rangos como [Thursday .. Sunday] ó [minBound .. maxBound] :: [Day]
--Bounded: tipos que tienen un valor mínimo y otro máximo, es decir, están acotados inferior y superiormente.
--podemos usar: minBound, maxBound (ver ejemplos de comentarios encima y debajo de esta línea)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

{-
ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday
-}

