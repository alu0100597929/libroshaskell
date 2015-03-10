{- capslocker.hs
import Control.Monad
import Data.Char
main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l
-}

{-
main = do
  contents <- getContents
  putStr (soloLineasCortas contents)

soloLineasCortas :: String -> String
soloLineasCortas = unlines . filter (\line -> length line < 10) . lines
-}

{-
main = interact informarPalindromas

informarPalindromas :: String -> String
informarPalindromas =
  unlines .
  map (\xs -> if esPal xs then "palíndroma" else "no palíndroma") .
  lines

esPal :: String -> Bool
esPal xs = xs == reverse xs
-}

{-
import System.IO

main = do
  handle <- openFile "agujero_negro.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
-}

{-import System.IO
import System.Directory
import Data.List

main = do
  contenido <- readFile "tareas.txt"
  let cosasQueHacer = lines contenido
      cosasNumeradas = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] cosasQueHacer
  putStrLn "Estas son tus cosas que hacer:"
  mapM_ putStrLn cosasNumeradas
  putStrLn "¿Cuál quieres borrar?"
  cadenaNumero <- getLine
  let numero = read cadenaNumero
      nuevasCosasQueHacer = unlines $ delete (cosasQueHacer !! numero) cosasQueHacer
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle nuevasCosasQueHacer
  hClose tempHandle
  removeFile "tareas.txt"
  renameFile tempName "tareas.txt"
-}

{-
import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
  contents <- readFile "tareas.txt"
  let cosasQueHacer = lines contents
      cosasNumeradas = zipWith (\n line -> show n ++ " - " ++ line)
                                   [0..] cosasQueHacer
  putStrLn "Estas son tus cosas que hacer:"
  mapM_ putStrLn cosasNumeradas
  putStrLn "¿Cuál quieres borrar?"
  cadenaNumero <- getLine
  let numero = read cadenaNumero
      nuevasCosasQueHacer = unlines $ delete (cosasQueHacer !! numero) cosasQueHacer
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle nuevasCosasQueHacer
      hClose tempHandle
      removeFile "tareas.txt"
      renameFile tempName "tareas.txt")
-}

{-import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [nombreFichero, cosaQueHacer] = appendFile nombreFichero ("\n" ++ cosaQueHacer ++ "\r")

view :: [String] -> IO ()
view [nombreFichero] = do
  contenido <- readFile nombreFichero
  let cosasQueHacer = lines contenido
      cosasNumeradas = zipWith (\n linea -> show n ++ " - " ++ linea)
                                  [0..] cosasQueHacer
  putStr $ unlines cosasNumeradas

remove :: [String] -> IO ()
remove [nombreFichero, numeroString] = do
  contenido <- readFile nombreFichero
  let cosasQueHacer = lines contenido
      cosasNumeradas = zipWith (\n linea -> show n ++ " - " ++ linea)
                                  [0..] cosasQueHacer
  let numero = read numeroString
      newTodoItems = unlines $ delete (cosasQueHacer !! numero) cosasQueHacer 
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile nombreFichero
      renameFile tempName nombreFichero)-}

--quedaron dos ejercicios pendientes sobre el programa anterior

{-
import System.Random

--recuerda, el let va indentado
tresMonedas :: StdGen -> (Bool, Bool, Bool)
tresMonedas gen =
  let (primeraMoneda, nuevoGen) = random gen
      (segundaMoneda, nuevoGen') = random nuevoGen
      (terceraMoneda, nuevoGen'') = random nuevoGen'
  in  (primeraMoneda, segundaMoneda, terceraMoneda)
-}

{-
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
-}

{-
--la implementación de randoms sería algo como esto, como la llamada
--recursiva necesita un nuevo generador, no podemos devolver sino una
--lista de valores 
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value:restOfList, finalGen)
-}

import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  preguntarNumero gen
  
preguntarNumero :: StdGen -> IO ()
preguntarNumero gen = do
  let (numAleatorio, nuevoGen) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "¿En qué número del 1 al 10 estoy pensando? "
  cadenaNumero <- getLine
  when (not $ null cadenaNumero) $ do
    case reads cadenaNumero :: [(Integer,String)] of
      [(n, _)] -> if numAleatorio == fromIntegral n
                    then putStrLn "¡Correcto!"
                    else putStrLn $ "Lo siento, era " ++ show numAleatorio
      _        -> putStrLn "invalid input"
    preguntarNumero nuevoGen