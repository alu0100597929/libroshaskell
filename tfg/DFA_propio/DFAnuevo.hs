module DFA where

import Control.Monad(forever, liftM)
import Data.List(foldl')

data DFA = DFA { estadoInicial :: String
               , estadosAceptacion :: [String]
               , tablaTransiciones :: [((String , Char), String)]
               }

transitar :: String -> Char -> [((String , Char), String)] -> String
transitar estado caracter table = case lookup (estado,caracter) table of
                                    Just str -> str
                                    Nothing -> ""

computarCadena' :: String -> [((String , Char), String)] -> String -> [String] -> Bool
computarCadena' [] _ estadoActual finales = estadoActual `elem` finales
computarCadena' (x:xs) table estadoActual finales = computarCadena' xs table (transitar estadoActual x table) finales

computarCadena :: String -> DFA -> Bool
computarCadena cadena (DFA {tablaTransiciones = table, estadoInicial = estadoActual, estadosAceptacion = finales}) =
  computarCadena' cadena table estadoActual finales

stringATransicion :: String -> ((String, Char), String)
stringATransicion str = let (x:y:z:[]) = words str
                        in ((x, head y), z)

leerFichero :: IO ()
leerFichero = do
  fileContents <- liftM lines $ readFile "dfa1.txt"
  let estadoInicio = head fileContents
      tablaBuena = map stringATransicion $ tail (init fileContents)
      estadosFinales = [last fileContents]
      dfa = DFA estadoInicio estadosFinales tablaBuena
  forever $ do
    putStr "Cadena:"
    cadena <- getLine
    if computarCadena cadena dfa
      then putStrLn "Cadena aceptada"
      else putStrLn "Cadena rechazada"

main :: IO ()
main = leerFichero