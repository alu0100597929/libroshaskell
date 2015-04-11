leerFichero :: String -> IO ()
leerFichero filename = do
                          contenidos <- readFile filename
                          putStrLn contenidos