{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.ByteString (ByteString) -- esta línea nos permite llamar al tipo ByteString en vez de BS.ByteString
import Data.Bits ( xor )
import Parser
import Data.String.Conversions (cs) -- para evitar los fallos de bytestrings lazy y normal
import Data.List ( find )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as M

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret ruta1 ruta2 = do
  fichero1 <- BS.readFile ruta1
  fichero2 <- BS.readFile ruta2
  xorFicheros <- return $ BS.pack $ BS.zipWith (xor) fichero1 fichero2
  cerosFuera <- return $ BS.filter (/= 0) xorFicheros
  return cerosFuera

-- TODO: mejorar la creación de la clave repetida para que coincida exactamente con el tamaño del fichero encriptado
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key rutaFichEncriptado = do
  contenidoCifrado <- BS.readFile rutaFichEncriptado
  let claveRepetida = BS.concat $ replicate (BS.length contenidoCifrado) key
      rutaGuardar = reverse $ drop 4 $ reverse rutaFichEncriptado
  xorClave <- return $ BS.pack $ BS.zipWith (xor) contenidoCifrado claveRepetida
  BS.writeFile rutaGuardar xorClave
  putStrLn $ "Fichero desencriptado en " ++ rutaGuardar

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile ruta = do
  byteStringFichero <- BS.readFile ruta
  descifrado <- return $ decode $ cs byteStringFichero
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
  filtradas <- return $ Just $ filter (esDeVictima (fromMaybe [] victimas)) (fromMaybe [] transacciones)
  return filtradas

hacerOperacion :: Transaction -> M.Map String Integer -> M.Map String Integer
hacerOperacion (Transaction { from = desde, to = hasta, amount = cantidad }) mapaViejo =
  let mapaConOperacionNegativa = M.insertWith (+) desde (negate cantidad) mapaViejo
  in M.insertWith (+) hasta cantidad mapaConOperacionNegativa

getFlow :: [Transaction] -> M.Map String Integer
getFlow xs = last $ getFlow' xs M.empty

getFlow' :: [Transaction] -> M.Map String Integer -> [M.Map String Integer]
getFlow' [] mapaViejo = [mapaViejo]
getFlow' (x:xs) mapaViejo = let mapaConOperacionHecha = hacerOperacion x mapaViejo
                            in mapaConOperacionHecha : getFlow' xs mapaConOperacionHecha

prueba = let ts = [ Transaction { from = "Haskell Curry"
                                , to = "Simon Peyton Jones"
                                , amount = 10
                                , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                                }
                  ]
         in getFlow ts

-- resumenTransacciones = getFlow 

main :: IO ()
main = do
  secreto <- getSecret "dog.jpg" "dog-original.jpg"
  print $ "El secreto es: " ++ show secreto
  putStrLn "Desencriptando..."
  decryptWithKey secreto "victims.json.enc"