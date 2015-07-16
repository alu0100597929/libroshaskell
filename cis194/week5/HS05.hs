{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import Data.ByteString (ByteString) -- esta línea nos permite llamar al tipo ByteString en vez de BS.ByteString
import Data.Bits ( xor )

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

main :: IO ()
main = do
  secreto <- getSecret "dog.jpg" "dog-original.jpg"
  print $ "El secreto es: " ++ show secreto
  putStrLn "Desencriptando..."
  decryptWithKey secreto "victims.json.enc"