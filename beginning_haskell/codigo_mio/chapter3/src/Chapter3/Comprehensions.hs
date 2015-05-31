{-# LANGUAGE TransformListComp #-}

module Chapter3.Comprehensions where

import Data.Char
import GHC.Exts -- viene en el paquete base

doubleOdds :: [Integer] -> [Integer]
doubleOdds list = map (*2) $ filter odd list

doubleOdds' :: [Integer] -> [Integer]
doubleOdds' xs = [2*x | x <- xs, odd x]

-- las list comprehensions tienen 3 partes, la primera es la expresión,
-- es decir, la transformación que se aplicará a los elementos que pasen la criba
-- la segunda está compuesta por generadores, que van asignando valores a nombres
-- la tercera es una lista de condiciones que deben cumplir los generador para
-- quedarse en la lista que posteriormente será transformada.

-- *Chapter3.Comprehensions> [ clientName x | x@(GovOrg _ _) <- listOfClients ]
-- ["NTTF"]

tablas_de_multiplicar = [(x,y,x*y) | x <- [1 .. 4], y <- [1 .. 10]]

domino_nonrep = [(x,y) | x <- [0..6], y <- [x..6]]

-- importante
-- esta comprensión inserta un espacio y luego cada elemento de s, para luego
-- pasar todo a mayúsculas, recuerda que va carácter a carácter
cosaRara = [ toUpper c | s <- ["This","is","a","list"], c <- ' ':s]

-- importante, let dentro de una list comprehension
-- aplicar la norma a un vector
normas = [ sqrt v | (x,y) <- [(1,2),(3,8)], let v = x*x+y*y ]

domino_guards = [(x,y) | x <- [1 .. 6], y <- [1 .. 6], x <= y]

-- TransformListComp
extension = [x*y | x <- [-1,1,-2], y <- [1,2,3], then reverse]

-- esto necesita import GHC.Exts
sortExts = [x*y | x <- [-1,1,-2], y <- [1,2,3], then sortWith by x]