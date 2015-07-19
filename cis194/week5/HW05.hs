{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.ByteString (ByteString) -- esta línea nos permite llamar al tipo ByteString en vez de BS.ByteString
import Data.Bits ( xor )
import Parser
import Data.String.Conversions (cs) -- para evitar los fallos de bytestrings lazy y normal
import Data.List ( find, maximumBy, sortBy )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as M

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret ruta1 ruta2 = do
  fichero1 <- BS.readFile ruta1
  fichero2 <- BS.readFile ruta2
  let xorFicheros = BS.pack $ BS.zipWith (xor) fichero1 fichero2
      cerosFuera = BS.filter (/= 0) xorFicheros
  return cerosFuera

-- TODO: mejorar la creación de la clave repetida para que coincida exactamente con el tamaño del fichero encriptado
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key rutaFichEncriptado = do
  contenidoCifrado <- BS.readFile rutaFichEncriptado
  let claveRepetida = BS.concat $ replicate (BS.length contenidoCifrado) key
      rutaGuardar = reverse $ drop 4 $ reverse rutaFichEncriptado
      xorClave = BS.pack $ BS.zipWith (xor) contenidoCifrado claveRepetida
  BS.writeFile rutaGuardar xorClave
  putStrLn $ "Fichero desencriptado en " ++ rutaGuardar

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile ruta = do
  byteStringFichero <- BS.readFile ruta
  let descifrado = decode $ cs byteStringFichero
  return descifrado

{-
*Main> parseFile "victims.json"
Nothing
*Main> parseFile "victims.json" :: IO (Maybe [TId])
Just ["42860e63-e035-49d1-a559-8f90deddf748","a76dffac-2cd8-4344-a5df-1107b944d980","8201b900-9b12-47ca-a9ab-280e8d32bd2b",
-}

-- aquí hacemos un reconocimiento de patrones con record syntax para extraer datos de un ADT
esDeVictima :: [TId] -> Transaction -> Bool
esDeVictima listaTIdsVictimas (Transaction { tid = tidTransaccion }) =
  case find (== tidTransaccion) listaTIdsVictimas of
    Nothing -> False
    _       -> True

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs rutaVictimas rutaTransacciones = do
  victimas <- parseFile rutaVictimas :: IO (Maybe [TId])
  transacciones <- parseFile rutaTransacciones :: IO (Maybe [Transaction])
  let filtradas = Just $ filter (esDeVictima (fromMaybe [] victimas)) (fromMaybe [] transacciones)
  return filtradas

-- si usamos insertWith con cosas que no existen, se ponen los valores dados y no se ejecuta
-- la función, pues no hay valor viejo!!!
doOperation :: Transaction -> M.Map String Integer -> M.Map String Integer
doOperation (Transaction { from = desde, to = hasta, amount = cantidad }) mapaViejo =
  let mapWithSubtract = if (M.member desde mapaViejo)
                          then M.insertWith (-) desde (cantidad) mapaViejo
                          else M.insert desde (negate cantidad) mapaViejo
  in M.insertWith (+) hasta cantidad mapWithSubtract

getFlow :: [Transaction] -> M.Map String Integer
getFlow xs = last $ getFlow' xs M.empty

getFlow' :: [Transaction] -> M.Map String Integer -> [M.Map String Integer]
getFlow' [] mapaViejo = [mapaViejo]
getFlow' (x:xs) mapaViejo = let mapaConOperacionHecha = doOperation x mapaViejo
                            in mapaConOperacionHecha : getFlow' xs mapaConOperacionHecha

prueba = let ts = [ Transaction { from = "Haskell Curry"
                                , to = "Simon Peyton Jones"
                                , amount = 10
                                , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                                }
                  ]
         in getFlow ts

resumenTransacciones :: IO (M.Map String Integer)
resumenTransacciones = do
  maybeTransacciones <- getBadTs "victims.json" "transactions.json"
  let flujo = getFlow $ fromMaybe [] maybeTransacciones
  return flujo

mayorPar :: (Ord v) => (k,v) -> (k,v) -> Ordering
mayorPar (clave, valor) (clave2, valor2) = compare valor valor2

-- en los mapas NO puedo buscar por clave, se pasa a lista y se opera
getCriminal' :: M.Map String Integer -> String
getCriminal' mapa = do
  let listaPares = M.toList mapa
      parCriminal = maximumBy mayorPar listaPares
  fst parCriminal

getCriminal :: IO String
getCriminal = do
  mapa <- resumenTransacciones
  return $ getCriminal' mapa

--separate :: IO (M.Map String Integer) -> ([String,Integer],[String,Integer])
crearTransaccionesInversas = do
  mapa <- resumenTransacciones
  let listaRobados = M.toList $ M.filter (< 0) mapa
      listaLadrones = M.toList $ M.filter (> 0) mapa
      robadosOrdenados = sortBy mayorPar listaRobados
      ladronesOrdenados = sortBy (flip mayorPar) listaLadrones
  return $ crearNuevasTransacciones ladronesOrdenados robadosOrdenados

crearNuevasTransacciones :: [(String, Integer)] -> [(String, Integer)] -> [Transaction]
crearNuevasTransacciones [] _ = []
crearNuevasTransacciones _ [] = []
crearNuevasTransacciones (x:xs) (y:ys) =
  let (primero, segundo) = x
      (primero2, segundo2) = y
      cantidadAPagar = if segundo >= (negate segundo2) then negate segundo2 else segundo
      segundaLista = if (cantidadAPagar == negate segundo2) then ys else (y:ys)
  in if (cantidadAPagar == segundo)
       then Transaction { from = primero, to = primero2, amount = cantidadAPagar, tid = "" } : crearNuevasTransacciones xs segundaLista
       else Transaction { from = primero, to = primero2, amount = cantidadAPagar, tid = "" } : crearNuevasTransacciones (x:xs) segundaLista

-- cambiar las cosas estilo x <- return y por let x = y
-- catMaybes para evitar los fromMaybes!!!

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON rutaEscritura contenido = do
  let codificado = encode contenido
  BS.writeFile rutaEscritura $ cs codificado

main :: IO ()
main = do
  secreto <- getSecret "dog.jpg" "dog-original.jpg"
  print $ "El secreto es: " ++ show secreto
  putStrLn "Desencriptando..."
  decryptWithKey secreto "victims.json.enc"
  transacciones <- crearTransaccionesInversas
  writeJSON "new-transactions.json" transacciones
  putStrLn "Nuevas transacciones escritas en new-transactions.json" 
  nombreHacker <- getCriminal
  print $ "El hacker es: " ++ nombreHacker 