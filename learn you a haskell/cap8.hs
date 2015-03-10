import Data.Char
{-
main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " "
                    ++ bigLastName
                    ++ ", how are you?"
-}

main = do
  linea <- getLine
  if null linea
    then return ()
    else do --unimos acciones E/S en un bloque do porque después del else sólo puede haber una acción E/S
      putStrLn $ invertirPalabras linea
      main

invertirPalabras :: String -> String
invertirPalabras = unwords . map reverse . words

