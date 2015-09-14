-- NFA format file
-- 1st line: set of initial states
-- last line: set of final states
-- other lines: transitions table entries, each one is of the form:
-- (state, char viewed, set of next states)

-- importante, un NFA es "paralelo", pero cada estado no tiene
-- información de lo que están haciendo los otros, por ello las
-- transiciones son del tipo State -> Char -> [State]

import Control.Monad(liftM, forever)

data NFA = NFA {
                  conjuntoInicial :: [String],
                  tablaTransiciones :: [((String, Char), [String])],
                  conjuntoAceptacion :: [String]
               }

stringToTransition :: String -> ((String, Char), [String])
stringToTransition str = let (x:y:z) = words str
                         in ((x, head y), z)

transitar :: [((String, Char), [String])] -> Char -> String -> [String]
transitar table car str = case lookup (str, car) table of
                            Just xs -> xs
                            Nothing -> []

computar'' :: String -> [((String, Char), [String])] -> [String] -> [String] -> [String]
computar'' [] _ actual _ = actual 
computar'' (x:xs) tabla actual aceptacion = computar'' xs tabla (actual >>= (transitar tabla x)) aceptacion

computar' :: String -> NFA -> [String]
computar' (x:xs) (NFA { conjuntoInicial = inicial, tablaTransiciones = tabla, conjuntoAceptacion = aceptacion }) = 
  computar'' xs tabla (transitar tabla x (head inicial)) aceptacion

computar :: String -> NFA -> Bool
computar cad nfa@(NFA { conjuntoAceptacion = aceptacion }) =
  let conjuntoComputado = computar' cad nfa 
  in or $ map (\x -> x `elem` aceptacion) conjuntoComputado

leerFichero = do
  fileContents <- liftM lines $ readFile "nfa1.txt"
  let conjuntoInicio = words $ head fileContents
      tablaMala = init $ tail fileContents
      tablaBuena = map stringToTransition tablaMala
      conjuntoFinales = words $ last fileContents
      nfa = NFA conjuntoInicio tablaBuena conjuntoFinales
  -- putStrLn $ "Conjunto inicial: " ++ show conjuntoInicio ++ "\nTabla transiciones: " ++ show tablaBuena
  --            ++ "\nConjunto finales: " ++ show conjuntoFinales
  forever $ do
    putStr "Cadena:"
    cadena <- getLine
    case computar cadena nfa of
      False -> putStrLn "Cadena rechazada"
      True -> putStrLn "Cadena aceptada"

main = leerFichero