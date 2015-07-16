-- IO a, como tipo es puro, es la descripción de una computación
-- hay que distinguir entre una tarta y su receta. La receta una vez ejecutada correctamente
-- nos dará una tarta, pero, como receta, es un simple conjunto de instrucciones puras

-- c :: Cake
-- r :: Recipe Cake

-- Por ello, un valor IO String NO contiene una String!! Sino que contiene la descripción de
-- una computación que, cuando se ejecute, dará una cadena

--putStrLn :: String -> IO ()
main = putStrLn "Hello, Haskell!"

sillyExchange :: IO ()
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"

-- como todo tiene que producir ALGO en haskell, algunas acciones IO producen la tupla
-- vacía o unidad (), se parece un poco a una función C que devuelve void

jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- discard title
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest

-- data D = C T1 T2 T3
data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }