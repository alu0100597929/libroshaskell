{-
A parser for things
Is a function from strings
To lists of pairs
Of things and strings
-}
type Parser a = String -> [(a, String )]

--recibe una entrada y devuelve un parser con v y la entrada
return' :: a -> Parser a
return' v = \inp -> [(v , inp)]

--siempre falla
failure' :: Parser a
failure' = \inp -> [ ]

--falla con lista vacía, o crea un parser con el primer caracter como re
--sultado
item :: Parser Char
item = \inp -> case inp of
                  [] -> []
                  (x : xs) -> [(x , xs)]
		  
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x , y)
