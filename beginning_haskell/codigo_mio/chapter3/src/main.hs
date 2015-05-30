{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

import Chapter3.ParamPoly (Client(..), Person(..))
import Data.List

-- *Main> getClientName (GovOrg 189 "pepito")
-- "pepito"

getClientName :: Client a -> String
getClientName (GovOrg { clientId, clientName })                                  = clientName
getClientName (Company { clientId, clientName, person, duty})                    = clientName
getClientName (Individual { clientId, person = Person { firstName, lastName } }) = firstName ++ " " ++ lastName

-- el "infinito" para cadenas, una cadena muy larga
longStr = take 50000 $ repeat 'A'

clienteLargo :: a -> Client a
clienteLargo x = Individual { clientId = x, person = Person {firstName = longStr, lastName = ""} }

compareClients :: Client a -> Client a -> Client a
compareClients cli1 cli2 = if getClientName cli1 > getClientName cli2
                             then cli1
                             else cli2

-- la cabecera comentada no funciona
-- minimumClient' :: [Client a] -> Client a
minimumClient' :: [Client String] -> Client String
minimumClient' = foldr compareClients (clienteLargo "topor")

client1 = GovOrg "Bill Gates" "NTTF"
client2 = Individual { clientId = "pepito", person = Person { firstName = "josito", lastName = "yoksetioxdxd" } }

minimumBy' :: (Ord b) => (a -> b) -> [a] -> a
minimumBy' f xs = case indice of
                   Just i -> xs !! i
                   Nothing -> error "error sano"
  where
    indice          = findIndex (==minim) lista_procesada
    minim           = minimum lista_procesada
    lista_procesada = map f xs

minimumClient :: [Client a] -> Client a
minimumClient = minimumBy' getClientName

bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
bothFilters p list = (filter p list, filter (not . p) list)

-- la función de arriba es correcta, sin embargo, va a pasar por la lista dos veces
-- partition hace lo mismo en sólo una pasada

parti = partition (>0) [1,2,-3,4,-5,-6]

-- find encuentra el primer elemento directamente en una lista
ejemploFind1 = find (> 0) [1,2,-3,4,-5,-6]

ejemploFind2 = find (> 7) [1,2,-3,4,-5,-6]

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

takeUntilStop :: [String]
takeUntilStop = takeWhile (/= "stop") ["hello", "send", "stop", "receive"]

spanInStop :: ([String],[String])
spanInStop = span (/= "stop") ["hello", "send", "stop", "receive"]

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkIndividualAnalitics :: [Client a] -> (Bool, Bool)
checkIndividualAnalitics cs = (any isIndividual cs, not $ all isIndividual cs)

{-
*Main> nubBy (\x y -> (even x && even y) || (odd x && odd y)) [1,2,3,4,5]
[1,2]
Por tanto nubBy lo que hace es devolvernos una lista en la cual no sea posible
que se de la condición
*Main> nubBy (\x y -> x+y /= y) [0,1..10]
[0,1]
*Main> nubBy (\x y -> x+y == 2*y) [0,1..10]
[0,1,2,3,4,5,6,7,8,9,10]
*Main> nubBy (\x y -> x+y == y-1) [0,1..10]
[0,1,2,3,4,5,6,7,8,9,10]
*Main> nubBy (\x y -> x+y == y+1) [0,1..10]
[0,1]
-}

conjuntos = let x1 = [1,2,3,4]
                x2 = [2,3,5]
            in (x1 `union` x2, x1 `intersect` x2, x1 \\ x2)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) = e == x || elem' e xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' e xs = case find (==e) xs of
                Just x -> True
                _      -> False

