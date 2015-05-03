import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readOct, readDec, readHex, readInt, readFloat)
import Data.Char (digitToInt)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

{-
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"
-}

--las funciones pueden ser pasadas a funciones, y las acciones pueden ser pasadas a acciones:
--pasándole space a skipMany1 conseguimos un Parser que reconocerá uno o varios espacios.
spaces :: Parser ()
spaces = skipMany1 space

{-En la mónada Parser, el operador de ligado >> significa: intente usar el primer parser, luego
intenta usar el otro con la entrada que quede por parsear, y falla si ambos fallan-}
--la función fallará si no ponemos ni un sólo espacio en blanco antes de los carácteres de symbol
{-
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"
-}

main :: IO ()
main = do 
  args <- getArgs
  putStrLn (readExpr (args !! 0))

--constructor algebraico del tipo LispVal, cada uno de los constructores de valor tiene una
--etiqueta (siempre empiezan por mayúscula) y el tipo de dato que recibe para construir el
--valor tipo LispVal

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Floating Float
             | String String
             | Bool Bool
             | Character Char

{-
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x
-}

{-
Desafortunadamente, el resultado de many1 digit es realmente un
Parser String, así que nuestra composición Number . read todavía no
puede operar sobre él. Necesitamos una manera de decirle que
simplemente opere con el valor dentro de la mónada, devolviéndonos
un Parser LispVal. Eso es lo que hace liftM, así que aplicamos liftM
a nuestra función Number . read y después aplicamos el resultado de
eso a nuestro parser.

No olvidemos import Control.Monad

--many1 casa con uno o muchos de sus argumentos, en este caso uno o muchos dígitos
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
-}

{-
El operador decisión: <|> intenta primero un parser y luego el otro, devolviendo el valor
del parser que tuvo éxito. El primer parser debe fallar antes de consumir entrada. Luego
veremos cómo se implementa el backtracking
first es un elemento
rest es una lista, por ello usamos (:) (a parte de por la eficiencia)-}
parseAtom :: Parser LispVal
parseAtom = do
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool = do
          char '#'
          c <- oneOf "tf"
          return $ case c of
                 't' -> Bool True
                 'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n

bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> x*(2^i)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

parseOctal :: Parser LispVal
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

readWith f s = fst $ f s !! 0 

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseChar 
        <|> parseString 
        <|> parseNumber
        <|> parseBool
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

--recuerda: \\ es la barra simple, sólo que se debe escapar
parseString :: Parser LispVal
parseString = do
                char '"'
                s <- many (escapedChars <|> (noneOf ['\\', '"']))
                char '"'
                return $ String s

escapedChars :: Parser Char
escapedChars = do
                 char '\\'
                 c <- oneOf ['\\','"', 'n', 'r', 't']
                 return $ case c of
                            '\\' -> c
                            '"'  -> c
                            'n'  -> '\n'
                            'r'  -> '\r'
                            't'  -> '\t'

strToChar :: String -> Char
strToChar = (!! 0)

parseChar :: Parser LispVal
parseChar = do
    sequence [char '#', char '\\']
    resultado <- many1 (letter <|> symbol)
    return $ case resultado of
               "space" -> Character ' '
               "newline" -> Character '\n'
               str -> Character $ strToChar str

-- s: short, f: float, d: double, l: long
parseFloat :: Parser LispVal
parseFloat = do
              n <- many $ oneOf "0123456789.SFDL"
              (return . Floating . (readWith readFloat)) n

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]