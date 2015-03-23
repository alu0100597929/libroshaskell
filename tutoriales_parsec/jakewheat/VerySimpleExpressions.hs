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

-- el parser de SingleAdd preparado para espacios
addL :: Parser SingleAdd
addL = do
    sumando1 <- lexeme $ many1 digit
    _ <- lexeme $ char '+'
    sumando2 <- lexeme $ many1 digit
    return $ SingleAdd (read sumando1) (read sumando2)

data SimpleExpr = Num Integer
                | Var String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                  deriving (Eq, Show)

simpleExprExamples :: [(String,SimpleExpr)]
simpleExprExamples =
    [("a", Var "a")
    ,("1", Num 1)
    ,("2 + 3", Add (Num 2) (Num 3))
    ,("(42)", Parens (Num 42))]

numE :: Parser SimpleExpr
numE = do
    x <- lexeme $ many1 digit
    return $ Num (read x)

varE :: Parser SimpleExpr
varE = lexeme $ do
    pc <- primerCar
    resto <- many noPrimerCar
    return $ Var (pc:resto)
  where
    primerCar = satisfy (\a -> isLetter a || a == '_')
    noPrimerCar = satisfy (\a -> isLetter a || isDigit a || a == '_')

parensE :: Parser SimpleExpr
parensE = do
    void $ lexeme $ char '('
    e <- numE
    void $ lexeme $ char ')'
    return $ Parens e

addE :: Parser SimpleExpr
addE = lexeme $ do
    e0 <- numE
    _ <- lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1

numOrVar :: Parser SimpleExpr
numOrVar = numE <|> varE

simpleExpr :: Parser SimpleExpr
simpleExpr = numE <|> varE <|> addE <|> parensE

simpleExpr1 :: Parser SimpleExpr
simpleExpr1 = addE <|> numE <|> varE <|> parensE

simpleExpr2 :: Parser SimpleExpr
simpleExpr2 = try addE <|> numE <|> varE <|> parensE

parensE3 :: Parser SimpleExpr
parensE3 = do
    void $ lexeme $ char '('
    e <- simpleExpr3
    void $ lexeme $ char ')'
    return $ Parens e

addE3 :: Parser SimpleExpr
addE3 = do
    e0 <- simpleExpr3
    void $ lexeme $ char '+'
    e1 <- simpleExpr3
    return $ Add e0 e1

simpleExpr3 :: Parser SimpleExpr
simpleExpr3 = try addE3 <|> numE <|> varE <|> parensE3

parensE4 :: Parser SimpleExpr
parensE4 = do
    void $ lexeme $ char '('
    e <- simpleExpr4
    void $ lexeme $ char ')'
    return $ Parens e

simpleExpr4 :: Parser SimpleExpr
simpleExpr4 = numE <|> varE <|> parensE4

parensEN :: Parser SimpleExpr -> Parser SimpleExpr
parensEN simpleExprImpl = do
    void $ lexeme $ char '('
    e <- simpleExprImpl
    void $ lexeme $ char ')'
    return $ Parens e

term :: Parser SimpleExpr -> Parser SimpleExpr
term simpleExprImpl = numE <|> varE <|> parensEN simpleExprImpl

term5 :: Parser SimpleExpr
term5 = term term5

addE5 :: Parser SimpleExpr
addE5 = do
    e0 <- term5
    void $ lexeme $ char '+'
    e1 <- term5
    return $ Add e0 e1

simpleExpr5 :: Parser SimpleExpr
simpleExpr5 = try addE5 <|> term5

term6 :: Parser SimpleExpr
term6 = term simpleExpr6

addE6 :: Parser SimpleExpr
addE6 = do
    e0 <- term6
    void $ lexeme $ char '+'
    e1 <- simpleExpr6
    return $ Add e0 e1

simpleExpr6 :: Parser SimpleExpr
simpleExpr6 = try addE6 <|> term6

term7 :: Parser SimpleExpr
term7 = term simpleExpr7

simpleExpr7 :: Parser SimpleExpr
simpleExpr7 = do
    -- first parse a term
    e <- term7
    -- then see if it is followed by an '+ expr' suffix
    maybeAddSuffix e
  where
    -- this function takes an expression, and parses a
    -- '+ expr' suffix, returning an Add expression
    -- it recursively calls itself via the maybeAddSuffix function
    addSuffix e0 = do
        void $ lexeme $ char '+'
        e1 <- term7
        maybeAddSuffix (Add e0 e1)
    -- this is the wrapper for addSuffix, which adapts it so that if
    -- addSuffix fails, it returns just the original expression
    maybeAddSuffix e = addSuffix e <|> return e

simpleExpr8 :: Parser SimpleExpr
simpleExpr8 = chainl1 term8 op
  where
    op = do
        void $ lexeme $ char '+'
        return Add
    term8 = term simpleExpr8