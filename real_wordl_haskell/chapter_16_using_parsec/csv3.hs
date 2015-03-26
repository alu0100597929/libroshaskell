-- file: ch16/csv3.hs
import Text.ParserCombinators.Parsec

-- This function is not correct!
eol = string "\n" <|> string "\n\r"

parseCSV :: String -> Either ParseError Char
parseCSV input = parse eol "(unknown)" input