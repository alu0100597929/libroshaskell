import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

-- Parser que devuelve un Bool, siempre True
alwaysTrue :: Parser Bool
alwaysTrue = pure True

-- Parser String: un parser que devuelve una String
-- Como vemos, parsec devuelve un Either, Left si da error, Right si todo va bien
matchTrue :: Parser String
matchTrue = string "true"

matchFalse :: Parser String
matchFalse = string "false"

-- ahora queremos combinar los dos tipos, y recibiendo una String,
-- queremos devolver un Bool, necesitamos (*>) "star arrow"
-- (*>) primero intenta lo de la izq., si tiene éxito, ejecuta lo de
-- la dcha y lo devuelve. Si no, devuelve lo que dé (error) el de la izq.

{- En resumen, devuelve aquello a lo que la flecha apunte, si todo va bien.-}
-- *> :: Applicative f => f a -> f b -> f b
-- *> :: Parser a => Parser a -> Parser b -> Parser b
boolTrue :: Parser Bool
boolTrue = matchTrue *> alwaysTrue
-- boolTrue = (string "true") *> (pure True)

boolFalse :: Parser Bool
boolFalse = matchFalse *> alwaysTrue

-- operador "choice" (<|>) intenta los parsers en orden, hasta que uno tenga
-- éxito o todos fallen
bool :: Parser Bool
bool = boolTrue <|> boolFalse

-- char :: Parser Char
-- noneOf :: [Char] -> Parser Char
-- many :: Parser p -> Parser [p]
-- es decir, many aplica muchas veces (o ninguna, es la * de las regexpr)
-- el parser que le digamos y pasa los resultados a una lista
stringLiteral :: Parser String
stringLiteral =
  char '"' *> (many (noneOf ['"'])) <* char '"'

{-
parse stringLiteral "test" "\"hello\""
parse stringLiteral "test" "true"
parse stringLiteral "test" "\"true\""
-}

-- la siguiente línea da error de tipos
-- value = bool <|> stringLiteral
-- solución: crear un tipo algebraico que contenga Bool y String

data JSONValue = B Bool
               | S String
               | A [JSONValue] --array de JSON
               | O [(String, JSONValue)]
               deriving Show

-- parser principal, usamos spaces al principio, porque es lo
-- único que da fallo con los lexeme parsers, el principio con espacios.
jsonValue :: Parser JSONValue
jsonValue = spaces >> (jsonBool <|> jsonStringLiteral <|> jsonArray
                           <|> jsonObject )

-- pero esto también da error de tipos porque recordemos:
-- bool :: Parser Bool
-- stringLiteral :: Parser String
-- solución: usar la clase de tipos Applicative
-- ($) :: Functor f => (a -> b) -> f a -> f b
-- ($) :: (a -> b) -> Parser a -> Parser b
-- map :: (a -> b) -> [a] -> [b]

jsonBool' :: Parser JSONValue
jsonBool' = B <$> bool
-- jsonBool = fmap B bool

-- jsonStringLiteral :: Parser JSONValue
-- jsonStringLiteral = S <$> stringLiteral
-- jsonStringLiteral = fmap S stringLiteral

jsonStringLiteral :: Parser JSONValue
jsonStringLiteral = lexeme (S <$> stringLiteral)

{-
parse jsonBool "test" "true"
parse jsonBool "test" "false"
parse jsonBool "test" "lemon"
-}

{-
jsonValue :: Parser JSONValue
jsonValue = jsonBool <|> jsonStringLiteral
-}

{-
parse jsonValue "test" "\"hello\""
parse jsonValue "test" "true"
-}

array :: Parser [JSONValue]
array =
  (lexeme $ char '[')
  *>
  ( jsonValue `sepBy` (lexeme $ char ',') )
  <*
  (lexeme $ char ']')

{-
parse array "test" "[\"ola\",\"k\",\"ase\"]"
Right [S "ola",S "k",S "ase"]
-}

jsonArray :: Parser JSONValue
jsonArray = A <$> array

jsonObject :: Parser JSONValue
jsonObject = O <$> ((lexeme $ char '{') *>
                    (objectEntry `sepBy` (lexeme $ char ','))
                    <* (lexeme $ char '}'))

objectEntry :: Parser (String, JSONValue)
objectEntry = do 
  key <- lexeme stringLiteral 
  char ':'
  value <- lexeme jsonValue
  return (key, value)

{-
*Main> parse objectEntry "test" "\"beer\":true"
Right ("beer",B True)
*Main> parse objectEntry "test" "\"beer\":true"
Right ("beer",B True)
-}

-- lidiemos con espacios intercalados usando lexeme

ws :: Parser String
ws = many (oneOf " \t\n")

-- mi propio combinador lexeme
lexeme :: Parser a -> Parser a
lexeme p = p <* ws

jsonBool = lexeme jsonBool'

-- el resto de Parsers ya fueron arreglados para usar lexeme.
-- es importante darse cuenta de que lexeme debe usarse en cada uno
-- de los Parsers más simples

{-
*Main> parse jsonValue "test" "[true, true, true]"
Right (A [B True,B True,B True])
-}