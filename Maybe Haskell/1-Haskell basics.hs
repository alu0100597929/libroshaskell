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

--data Persona = CrearPersona String Int
--     |         |
--     |         Constructor de datos
--     |
--     Constructor de tipos

-- Si solo hay un constructor de datos, podemos llamarlo igual que
-- el de tipo, ya que es imposible sustituirlos sintácticamente
data Persona = Persona String Int

fran :: Persona
fran = Persona "Fran" 25

getNombre :: Persona -> String
getNombre (Persona nombre _) = nombre

getEdad :: Persona -> Int
getEdad (Persona _ edad) = edad

