module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hola, " ++ args !! 0 ++ ", me alegro de que estés en " ++ args !! 1)