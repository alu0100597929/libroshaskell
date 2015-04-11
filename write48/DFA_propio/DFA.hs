-- DFA format file
-- 1st line: initial state
-- last line: set of final states
-- other lines: transitions table entries

module DFA where

  data DFA = DFA { intialState :: String
                 , isAccepting :: String -> Bool
                 , transition  :: String -> Char -> String
                 }

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
                      let lineas = lines $ contenidos
                          i = head lineas
                          a = (`elem` last (map words lineas))
                          dfa = DFA i a (t (strToRow (tail (init lineas))))
                      print $ testDFA dfa cadena

  -- currificada para usar foldl
  t :: [((String, Char), String)] -> String -> Char -> String
  t tab n c = case lookup (n,c) tab of
      Just x -> x
      _      -> error "transición errónea"

  testDFA :: DFA -> [Char] -> Bool
  testDFA (DFA i a t) = a . foldl t i