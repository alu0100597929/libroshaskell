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