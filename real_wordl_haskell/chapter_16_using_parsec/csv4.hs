-- file: csv4.hs
import Text.ParserCombinators.Parsec

-- This function is not correct!
eol = string "\n\r" <|> string "\n"

parseCSV :: String -> Either ParseError [Char]
parseCSV input = parse eol "(unknown)" input