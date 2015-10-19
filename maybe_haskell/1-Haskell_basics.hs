import Data.List

five :: Int
five = 5

almostThird :: Float
almostThird = (3 :: Float) / 9

actualThird :: Rational
actualThird = (3 :: Rational) / 9

add :: Int -> Int -> Int
add x y = x + y

-- estos paréntesis en la declaración de tipos significan agrupación, no aplicación
twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

-- podemos usar las comillas como secciones si queremos
intersects xs ys = any (`elem` xs) ys

-- Lambdas
dosVeces :: Int
dosVeces = twice (\x -> x * x + 10) 5

-- proceso para pasar de una lambda a una función con nombre
-- ejemplo: twice (\x -> x * x + 10)

-- coger la lambda
-- \x -> x * x + 10

-- darle un nombre
-- f = \x -> x * x + 10

-- sustituir "\--- ->" con argumentos normales
-- f x = x * x + 10

-- usar el nombre en vez de la lambda
-- twice f 5

-- Nuestros propios tipos de datos
--data Persona = CrearPersona String Int
--                            |      |
--                            |      |
--                            |      La edad de la persona
--                            El nombre de la persona

-- A la izquierda del igual está el constructor de tipos.
-- A la derecha del igual están los constructores de datos.
-- El constructor de tipos es el nombre del tipo y usado en las declaraciones de tipos.
-- Los constructores de datos son funciones que producen valores del tipo dado.

--data Persona = Persona String Int
--     |         |
--     |         Constructor de datos
--     |
--     Constructor de tipos

-- Si solo hay un constructor de datos, podemos llamarlo igual que
-- el de tipo, ya que es imposible sustituirlos sintácticamente
--data Persona = Persona String Int
data Persona a = PersonaConCosa String a | PersonaSinCosa String
--           |                         |
--           |                         podemos usarla aquí
--           |
--           Añadiendo una "variable de tipo" aquí

franConEdad :: Persona Int
franConEdad = PersonaConCosa "fran" 25

franSinEdad :: Persona Int
franSinEdad = PersonaSinCosa "fran"

getNombre :: Persona Int -> String
getNombre (PersonaConCosa nombre _) = nombre
getNombre (PersonaSinCosa nombre)   = nombre

getEdad :: Persona Int -> Maybe Int
getEdad (PersonaConCosa _ edad) = Just edad
getEdad (PersonaSinCosa _)      = Nothing

-- Incluso en el caso en el cual no añadimos la edad, especificamos
-- el tipo de aquello que no tenemos. Esa "a" podría ser cualquier tipo:

franConCorreo :: Persona String
franConCorreo = PersonaConCosa "fran" "freinn@gmail.com"

franSinCorreo :: Persona String
franSinCorreo = PersonaSinCosa "fran"

-- Cuando usamos el constructor de persona que no usa el parámetro de tipo
-- a, estamos siempre creando el mismo valor. Por ello, podemos definir una
-- función que genere un valor "sin cosa" y que funcione para trabajar con
-- Persona "CualquierTipo":

franSinCosa :: Persona a
franSinCosa = PersonaSinCosa "fran"

-- dar un valor por defecto es una mala decisión de diseño
dobleEdad :: Persona Int -> Int
dobleEdad (PersonaConCosa _ age) = 2 * age
dobleEdad (PersonaSinCosa _) = 1

-- por suerte, Maybe arregla este problema
dobleEdad' :: Persona Int -> Maybe Int
dobleEdad' (PersonaConCosa _ edad) = Just (2 * edad)
dobleEdad' (PersonaSinCosa _)      = Nothing

-- La gran utilidad de Maybe es pasar de funciones parciales a
-- funciones totales.

-- | Find the first element from the list for which the predicate function
-- returns True. Return Nothing if there is no such element.
find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' predicate (first:rest) =
  if predicate first
    then Just first
    else find' predicate rest

--
-- Warning: this is a type error, not working code!
--
-- findUser :: UserId -> User
-- findUser uid = find (matchesId uid) allUsers

{-
findUser :: UserId -> User
findUser uid =
  case find (matchesId uid) allUsers of
    Just u -> u
    Nothing -> -- what to do? error?
-}