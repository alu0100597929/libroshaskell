{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.FnsParams where

-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f n = 3 * f (n + 2)

-- ($) :: (a -> b) -> a -> b
-- f $ a = f a

maxi = maximum (map succ [1,2,3])

maxi' = maximum $ map succ [1,2,3]

mapeo = map (\x -> x+2) [1,2,3]

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples = map (\(x,y) -> x == y)

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                 "Alejandro" -> "Hello, writer"
                                 _           -> "Welcome, " ++ name
               ) names

-- necesita la extensión LambdaCase
sayHello' :: [String] -> [String]
sayHello' names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                      ) names

-- esto en realidad es una clausura
multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> x*n

mapeo2 = map (multiplyByN 5) [1..10]

-- exercise 3-2 Working with filters
filterOnes = filter ((==) 1)

filterANumber n = filter ((==) n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (not . p)

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName :: String }
            deriving Show

isGovOrg :: Client a -> Bool
isGovOrg (GovOrg { clientId, clientName }) = True
isGovOrg _                                 = False

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs []     = []
filterGovOrgs (x:xs) = case x of
                         (GovOrg { clientId, clientName }) -> x : filterGovOrgs xs
                         _                                 -> filterGovOrgs xs

govorg1 = GovOrg { clientId = 909, clientName = "Piter" }
govorg2 = GovOrg { clientId = 7, clientName = "James Bond" }
individual1 =  Individual { clientId = 2015, person = Person { firstName = "Pablo", lastName = "Iglesias" }}

{-
*Chapter3.FnsParams> filterGovOrgs [govorg1,govorg2,individual1 ]
[GovOrg {clientId = 909, clientName = "Piter"},GovOrg {clientId = 7, clientName = "James Bond"}]
-}