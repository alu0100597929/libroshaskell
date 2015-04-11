-- NFA format file
-- 1st line: set of initial states
-- last line: set of final states
-- other lines: transitions table entries, each one is of the form:
-- (state, char viewed, set of next states)

-- importante, un NFA es "paralelo", pero cada estado no tiene
-- información de lo que están haciendo los otros, por ello las
-- transiciones son del tipo String -> Char -> [String]

module NFA where

  import Control.Monad

  data NFA = NFA { intialStates :: [String]
                 , isAccepting :: String -> Bool
                 , transition :: String -> Char -> [String]
                 }

  strToRow :: [String] -> [((String, Char), [String])]
  strToRow str = map crea_tupla por_espacios
    where
      crea_tupla (x:y:xs) = ((x, head y), xs)
      por_espacios = map words str

  leerNFA :: String -> IO ()
  leerNFA filename = do
                      contenidos <- readFile filename
                      putStr "Cadena:"
                      cadena <- getLine
                      let lineas = lines $ contenidos
                          i = words $ head lineas
                          a = (`elem` last (map words lineas))
                          tab = strToRow $ tail $ init lineas
                          nfa = NFA i a (transitions tab)
                      print $ testNFA nfa cadena

  -- currificada para usar foldl
  transitions :: [((String, Char), [String])] -> String -> Char -> [String]
  transitions tab str c = case lookup (str,c) tab of
                            Just x -> x
                            _      -> error "no hay transición"

  -- necesita el épsilon '#' al principio de la cadena
  testNFA :: NFA -> String -> Bool
  testNFA (NFA i a t) inp = any a (i >>= \i0 -> foldM t i0 inp)