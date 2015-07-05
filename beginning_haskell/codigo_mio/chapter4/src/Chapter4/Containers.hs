{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ParallelListComp#-}

module Chapter4.Containers where

import Chapter4.TypeClasses
import qualified Data.Map as M
import qualified Data.Set as S

-- Map k a, k es el tipo de las keys y a es el tipo de lo que guarda el map
-- en Map, sólo puede haber un valor para una clave, con lo cual, si actualizamos,
-- el valor viejo se pierde

map1 = let m1 = M.singleton "hello" 3
           m2 = M.insert "bye" 2 m1
           m3 = M.insert "hello" 5 m2
           m4 = M.insertWith (+) "hello" 7 m3
       in (m1,m2,m3,m4)

map2 = M.fromList [("topor",0),("keylorsito",10)]

-- M.findWithDefault 0 "welcome" devuelve 0 si la clave "welcome" no está

-- Exercise 4-2

-- recibe el valor, la clave y el mapa
myInsert val = M.alter (\_ -> Just val)

{-
*Chapter4.Containers> let m5 = M.singleton "hello" 3
*Chapter4.Containers> myInsert 5 "topor" m5
fromList [("hello",3),("topor",5)]

M.alter (\(Just v) -> Just (v+7)) "hello" $ M.fromList [("hello", 3), ("bye", 4)]
-}

myDelete key = M.alter (\(Just v) -> Nothing) key

myAdjust f = M.alter (\(Just v) -> Just (f v))

map3 = let m1 = M.fromList [("topor",0),("keylorsito",10)]
           m2 = M.fromList [("topor",10),("chicharrito",10)]
       in (m1 `M.union` m2, M.intersectionWith (-) m1 m2)

mapYFoldr = let m = M.fromList [("hi",3),("bye",4)]
            in (M.map (*2) m, M.foldr (+) 0 m)

conjunto = let set1 = S.insert "welcome" $ S.singleton "hello"
               set2 = S.fromList ["hello","bye"]
            in ( set1 `S.intersection` set2
               , "welcome" `S.member` set1
               , S.map length set2)

-- exercise 4.3
data ClientKind = GovOrgKind
                | CompanyKind
                | IndividualKind