module DFA where

  data DFA = DFA { intialState :: String
                 , isAccepting :: String -> Bool
                 , transition  :: String -> Char -> String
                 }

  -- estado inicial
  i = "Q1"

  -- criterio de aceptación
  a = (`elem` ["Q1"])

  strToRow :: [String] -> [((String, Char), String)]
  strToRow str = map crea_tupla por_espacios
    where
      crea_tupla [x,y,z] = ((x, head y), z)
      por_espacios = map words str

  leerDFA :: String -> IO ()
  leerDFA filename = do
                      contenidos <- readFile filename
                      putStr "Cadena:"
                      cadena <- getLine
                      let table = strToRow . lines $ contenidos
                          dfa = DFA i a (t table)
                      print $ testDFA dfa cadena

  -- currificada para usar foldl
  t tab n c = case lookup (n,c) tab of
      Just x -> x
      _      -> error "transición errónea"

  testDFA :: DFA -> [Char] -> Bool
  testDFA (DFA i a t) = a . foldl t i