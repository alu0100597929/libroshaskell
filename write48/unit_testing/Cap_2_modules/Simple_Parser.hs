module Cap_2_modules.Simple_Parser where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex, readFloat)
import Data.Char (toLower)
import Data.Ratio ((%), Rational)
import Data.Complex (Complex (..))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
               deriving (Show)

{-main :: IO ()
main = do
     args <- getArgs
     putStrLn (readExpr (args !! 0))-}

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
         Left err -> "No match: " ++ show err
         Right val -> "Found value: " ++ show val

--
-- LispVal Parsers
--

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> try parseChar
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

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
parseDecimal = char 'd' >> parsePlainNumber

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
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

parseRatio :: Parser LispVal
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)

parseFloat :: Parser LispVal
parseFloat = do char '#'
                entera <- many digit
                char '.'
                decimal <- many1 (digit <|> oneOf "SFDLEsfdle")
                return $ Float (read(entera ++ "." ++ decimal))

parseComplex :: Parser LispVal
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 (return . String) s

parseChar :: Parser LispVal
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                      "space" -> Char ' '
                      "newline" -> Char '\n'
                      [x] -> Char x

parseBool :: Parser LispVal
parseBool = do char '#'
               c <- oneOf "tf"
               return $ case c of
                      't' -> Bool True
                      'f' -> Bool False

--
-- Helpers
--

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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

bin2Integer :: String -> Integer
bin2Integer s = sum $ map (\(i,x) -> x*(2^i)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

{-
*Main> :t readHex 
readHex :: (Eq a, Num a) => ReadS a
*Main> :t readWith 
readWith :: (t -> [(a, b)]) -> t -> a
-}
readWith f s = fst $ f s !! 0