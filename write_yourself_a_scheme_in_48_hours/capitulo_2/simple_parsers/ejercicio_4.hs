module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)
import Data.Char (toLower)

main :: IO ()
main = do
     args <- getArgs
     putStrLn (readExpr (args !! 0))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
            char '"'
            s <- many (escapedChars <|> (noneOf ['\\', '"']))
            char '"'
            return $ String s

escapedChars :: Parser Char
escapedChars = do
            char '\\'
            c <- oneOf ['\\','"','n','r','t']
            return $ case c of 
                       '\\' -> c 
                       '"' -> c
                       'n' -> '\n'
                       'r' -> '\r'
                       't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first:rest
          return $ Atom atom

parseBool :: Parser LispVal
parseBool = do char '#'
               c <- oneOf "tf"
               return $ case c of
                          't' -> Bool True
                          'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = (parsePlainNumber <|> parseRadixNumber)

parsePlainNumber :: Parser LispVal
parsePlainNumber = (many1 digit) >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = char 'd' >> parsePlainNumber

bin2Integer :: String -> Integer
bin2Integer s = sum $ map (\(i,x) -> x*(2^i)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2Integer) n

parseOctal :: Parser LispVal
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789ABCDEFabcdef"
              (return . Number . (readWith readHex)) n

{-
*Main> :t readHex 
readHex :: (Eq a, Num a) => ReadS a
*Main> :t readWith 
readWith :: (t -> [(a, b)]) -> t -> a
-}
readWith f s = fst $ f s !! 0

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseBool
        <|> parseString
        <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
         Left err -> "No match: " ++ show err
         Right val -> "Found value"