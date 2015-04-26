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

matchFalse :: Parser String
matchFalse = string "false"

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