import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char (oneOf, char, digit, satisfy)
import Text.Parsec.String.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import FunctionsAndTypesForParsing

numberExamples :: [(String, Integer)]
numberExamples = [("1", 1)
                 ,("23", 23)]

{-
*Main> regularParse (many digit) ""
Right ""
*Main> regularParse (many digit) "1"
Right "1"
*Main> regularParse (many digit) "daw"
Right ""
-}

-- si usamos many1, el fallo da un Left
num :: Parser Int
num = do
    digs <- many1 digit
    return (read digs)

-- si usamos many, el fallo da una excepción
num1 :: Parser Int
num1 = do
    digs <- many digit
    return (read digs)

var :: Parser String
var = do
    pc <- primerCar
    resto <- many noPrimerCar
    return (pc:resto)
  where
    primerCar = satisfy (\a -> isLetter a || a == '_')
    noPrimerCar = satisfy (\a -> isLetter a || isDigit a || a == '_')

data Parentheses = Parentheses Integer deriving (Eq, Show)

parensExamples :: [(String, Parentheses)]
parensExamples = [("(1)", Parentheses 1)
                 ,("(17)", Parentheses 17)]

parens :: Parser Parentheses
parens = do
    _ <- char '('
    entero <- many1 digit
    _ <- char ')'
    return $ Parentheses (read entero)

data SingleAdd = SingleAdd Integer Integer deriving (Eq, Show)

singleAddExamples :: [(String, SingleAdd)]
singleAddExamples = [("1+2", SingleAdd 1 2)
                    ,("101+202", SingleAdd 101 202)]

add :: Parser SingleAdd
add = do
    sumando1 <- many1 digit
    _ <- char '+'
    sumando2 <- many1 digit
    return $ SingleAdd (read sumando1) (read sumando2)

-- estos dos parsers funcionan, pero a medias, pues al introducir espacios
-- no son capaces de lidiar con ellos. Arreglémoslo.

-- este parser siempre tiene éxito
espaciosEnBlanco :: Parser ()
espaciosEnBlanco = void $ many $ oneOf " \n\t"

parensW :: Parser Parentheses
parensW = do
    espaciosEnBlanco
    _ <- char '('
    espaciosEnBlanco
    entero <- many1 digit
    espaciosEnBlanco
    _ <- char ')'
    espaciosEnBlanco
    return $ Parentheses $ read entero

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    espaciosEnBlanco
    return x

{-
parseConEspaciosEnBlanco :: Parser a -> String -> Either ParseError a
parseConEspaciosEnBlanco p = parseWithEof wrapper
  where
    wrapper = do
        espaciosEnBlanco
        p
-}

parseConEspaciosEnBlanco :: Parser a -> String -> Either ParseError a
parseConEspaciosEnBlanco p = parseWithEof (espaciosEnBlanco >> p)


-- many --> * en regexp
-- many1 --> + en regexp

parensL :: Parser Parentheses
parensL = do
    _ <- lexeme $ char '('
    entero <- lexeme $ many1 digit
    _ <- lexeme $ char ')'
    return (Parentheses (read entero))