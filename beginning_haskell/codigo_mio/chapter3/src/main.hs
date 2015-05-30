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
    indice = findIndex (==minim) lista_procesada
    minim = minimum lista_procesada
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

