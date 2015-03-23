import Control.Monad

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "¿Qué color asocias con el número "
             ++ show a ++ "?"
    getLine)
  putStrLn "Los colores que asociaste a 1, 2, 3 y 4 son: "
  mapM putStrLn colors