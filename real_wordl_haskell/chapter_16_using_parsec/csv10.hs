-- Versión que corrige algunos fallos del parser del libro
import Text.ParserCombinators.Parsec

-- sepEndBy, arreglo para que pille fin de fichero sin fin de línea
csvFile = sepEndBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"') -- importante, un try a la derecha? Sí, pues está dentro de un many y ahí se hace a la izquierda
                                          -- hay que ponerlo porque si no, falla al encontrar '",' cerrando una celda
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r