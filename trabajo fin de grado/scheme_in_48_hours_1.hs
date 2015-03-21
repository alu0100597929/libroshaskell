module Main where
import System.Environment

{-
--modificación 1 
main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hola, " ++ args !! 0 ++ ", me alegro de que estés en " ++ args !! 1)
-}

{-
--modificación 2 
main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Suma = " ++ show (read (args !! 0) + read (args !! 1)))
  putStrLn ("Resta = " ++ show (read (args !! 0) - read (args !! 1)))
  putStrLn ("Multiplicación = " ++ show (read (args !! 0) * read (args !! 1)))
  putStrLn ("División = " ++ show (read (args !! 0) / read (args !! 1)))
-}

--modificación 3
main :: IO ()
main = do
  args <- getArgs
  linea <- getLine
  putStrLn ("Hola, " ++ linea)