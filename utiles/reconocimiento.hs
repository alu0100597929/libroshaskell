-- funciones que dan la idea para pasar a C++ ciertos algoritmos
import Data.List

combinacionesSinRep :: [a] -> [[a]]
combinacionesSinRep [] = []
combinacionesSinRep (x:xs) = map (\e -> x:[e]) xs ++ combinacionesSinRep xs

partes :: [a] -> [[a]]
partes []     = [[]]
partes (x:xs) = partes xs ++ map (x:) (partes xs)

n_i :: [Int] -> [(Int,Int)]
n_i xs = map (\ys -> (head ys, length ys)) agrupada
  where agrupada = group $ sort xs

quitarDuplicados :: (Ord a) => [a] -> [a]
quitarDuplicados = map head . group . sort

frames :: [[Int]] -> [(Int,Int)]
frames xs = n_i . concat $ map quitarDuplicados xs

filtradoMedia xs = filter (\(x,y) -> fromIntegral y >= media) fotos
  where  
    media = fromIntegral (sum . map snd $ fotos) / fromIntegral (length fotos)
    fotos = frames xs