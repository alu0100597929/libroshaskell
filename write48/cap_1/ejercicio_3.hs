module Main where
import System.Environment (getArgs)

main :: IO ()
main = do putStrLn "¿Cómo te llamas?"
          nombre <- getLine
          putStrLn ("Hola, " ++ nombre)