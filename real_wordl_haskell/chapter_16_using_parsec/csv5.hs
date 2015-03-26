-- file: ch16/csv5.hs
import Text.ParserCombinators.Parsec

eol = 
    do char '\n'
       char '\r' <|> return '\n'

parseCSV :: String -> Either ParseError Char
parseCSV input = parse eol "(unknown)" input