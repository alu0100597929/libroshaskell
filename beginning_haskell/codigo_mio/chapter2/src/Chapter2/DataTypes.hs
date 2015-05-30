{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- lo de arriba es un pragma, un comentario especial que activa o desactiva un flag
-- en GHCi poner :set -XViewPatterns 

-- module Chapter2.DataTypes (ConnOptions(), connDefault) where
module Chapter2.DataTypes where

import Data.Char (toUpper)

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

-- como hay un sólo constructor de datos, se le pone el mismo nombre
data Person = Person String String Gender
            deriving Show

-- Exercise 2.4

data Gender = Male | Female | Unknown
            deriving Show

client1 = Individual (Person "Morsi" "Nebrera" Female) False
client2 = Individual (Person "Freinn" "Nodise" Male) False

data TimeMachine = TimeMachine Make ConcreteInfo
                 deriving Show
data Make = Make Manufacturer Model
          deriving Show
data ConcreteInfo = ConcreteInfo Name Travelfeatures Price
                  deriving Show
-- type representa sinónimos, que nos ayudan a definir las cosas mejor
type Manufacturer = String
type Model = String
type Name = String

type Travelfeatures = String
type Price = Double

timeMachine1 = TimeMachine (Make "ACME" "T3298-XP Pro") (ConcreteInfo "BetaModel1" "From the Big Bang to the End of Time" 100000000.5)
timeMachine2 = TimeMachine (Make "ACME" "XATMO-72") (ConcreteInfo "BetaModel2" "1 week in the past or the future from now" 100)

clientName :: Client -> String
clientName client = case client of
                      GovOrg name -> name
                      Company name _ _ _ -> name
                      Individual (Person name surname _) _ -> name ++ " " ++ surname

clientName' :: Client -> String
clientName' (GovOrg name)                          = name
clientName' (Company name _ _ _)                   = name
clientName' (Individual (Person name surname _) _) = name ++ " " ++ surname

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _ -> Nothing

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

-- las siguientes funciones NO son equivalentes

f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _ -> "There is no boss"
 
g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of
                 "Boss" -> name ++ " is the boss"
             _ -> "There is no boss"

{-
*Chapter2.DataTypes> f (Company "A" 5 (Person "John" "Jefferson" Male) "Director")
"There is no boss"
*Chapter2.DataTypes> g (Company "A" 5 (Person "John" "Jefferson" Male) "Director")
"*** Exception: /home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter2/src/Chapter2/Section2/DataTypes.hs:(67,16)-(68,49):
Non-exhaustive patterns in case
-}

{-El autor recalca bastante que el reconocimiento de patrones NO hace
backtracking cuando algo va mal en el cuerpo de un casamiento, si entra en
un caso, no evalúa luego los demás-}

data NumberOfEachGender = NumberOfEachGender (NumMales, NumFemales)
                        deriving Show

type NumMales = Integer
type NumFemales = Integer

{- Recordatorio:

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

-- como hay un sólo constructor de datos, se le pone el mismo nombre
data Person = Person String String Gender
            deriving Show

-- Exercise 2.4

data Gender = Male | Female | Unknown
            deriving Show
-}

getNumMales :: NumberOfEachGender -> Integer
getNumMales (NumberOfEachGender (m, _)) = m

getNumFemales :: NumberOfEachGender -> Integer
getNumFemales (NumberOfEachGender (_, f)) = f

-- por simplicidad, lo haré sólo con Individuals
countGenders :: [Client] -> NumberOfEachGender
countGenders []     = NumberOfEachGender (0,0)
countGenders (x:xs) = case x of
                        Individual (Person _ _ gender) _ ->
                          case gender of
                            Male   -> NumberOfEachGender (num_males + 1, num_females)
                            Female -> NumberOfEachGender (num_males, num_females + 1)
  where
    num_males   = getNumMales t
    num_females = getNumFemales t
    t           = countGenders xs

{-
*Chapter2.Section2.DataTypes> countGenders [client1, client2]
NumberOfEachGender (1,1)
*Chapter2.Section2.DataTypes> countGenders [client1, client1, client1, client2]
NumberOfEachGender (1,3)
-}

makeDiscount :: Double -> TimeMachine -> TimeMachine
makeDiscount discount (TimeMachine (Make a b) (ConcreteInfo c d price)) =
  (TimeMachine (Make a b) (ConcreteInfo c d (price * (discount / 100))))

discountMachines :: Double -> [TimeMachine] -> [TimeMachine]
discountMachines _ []            = []
discountMachines discount (x:xs) = makeDiscount discount x : discountMachines discount xs

discountMachines' discount = map (makeDiscount discount) 

[] +++ list2     = list2
(x:xs) +++ list2 = x:(xs +++ list2)

empty :: [a] -> Bool
empty [] = True
empty _  = False

head' :: [a] -> a
head' []    = error "lista vacía"
head' (x:_) = x

tail' :: [a] -> [a]
tail' []     = error "lista vacía"
tail' (x:xs) = xs

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = if x <= y
                    then sorted (y:zs)
                    else False

-- versión pro, del libro, mejorada por mí en el <=
sorted' [] = True
sorted' [_] = True
sorted' (x : r@(y:_)) = x <= y && sorted r

maxmin [x] = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

ifibonacci :: Integer -> Maybe Integer
ifibonacci n = if n < 0
                 then Nothing
                 else case n of
                        0 -> Just 0
                        1 -> Just 1
                        n -> let Just f1 = ifibonacci (n-1)
                                 Just f2 = ifibonacci (n-2)
                             in Just (f1 + f2)

-- lo comentado no sirve, debido a que no se puede repetir una variable
-- en el mismo patrón, se debe usar otra letra y comprobar su igualdad después, por ejemplo
-- o usar guardianes

{-
binom _ 0 = 1
binom x x = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)
-}

-- mío
binomialCoef n k
  | k == 0 || n == k = 1
  | otherwise = binomialCoef (n-1) (k-1) + binomialCoef (n-1) k

-- libro
binom _ 0 = 1
binom x y | x == y = 1
binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)

ifibonacci' n | n < 0 = Nothing
ifibonacci' 0 = Just 0
ifibonacci' 1 = Just 1
ifibonacci' n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                            in Just (f1 + f2)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise      = show n ++ " is a beautiful number"

specialMultiples' n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise = show n ++ " is a beautiful number"

-- Exercise 2.6

ack m n
  | m == 0          = n + 1
  | m > 0 && n == 0 = ack (m-1) 1
  | otherwise       = ack (m-1) (ack m (n-1))

unzip' :: [(a,b)] -> ([a],[b])
unzip' []      = ([],[])
unzip' [(x,y)] = ([x],[y])
unzip' xs = (map fst xs, map snd xs)

-- View patterns: lo que hacen es casar con un tipo, aplicarle una función
-- e intentar volver a casar con un posible resultado de dicha función

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

-- record syntax: provee mejor documentación, y permite usar los nombres de los
-- campos en cualquier orden, con lo cual hay más libertad.

data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String 
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

{-
*Chapter2.Section2.DataTypes> IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } }
IndividualR {person = PersonR {firstName = "John", lastName = "Smith"}}
*Chapter2.Section2.DataTypes> GovOrgR "NATO"
GovOrgR {clientRName = "NATO"}
-}

-- a parte, se crean funciones especiales para acceder a esos campos particulares:

{-
*Chapter2.Section2.DataTypes> clientRName (GovOrgR "NATO")
"NATO"
*Chapter2.Section2.DataTypes> :t duty 
duty :: ClientR -> String
-}

-- los nombres de constructores nunca pueden repetirse en un programa Haskell.
-- los nombres de los campos sí, pero sólo si ambos devuelven un valor del mismo tipo

-- al usar record syntax para los bindings, no estamos obligados a usar todos los campos
-- sólo aquellos que hagan falta para el pattern matching que deseemos

greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet CompanyR    { clientRName = c }                     = "Hello " ++ c
greet GovOrgR     { }                                     = "Welcome"

-- requiere NamedFieldPuns
greet' :: ClientR -> String
greet' IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName
greet' CompanyR    { clientRName }                    = "Hello " ++ clientRName
greet' GovOrgR     { }                                = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p

-- exercise 2.7

-- por ejemplo, la función makeR tiene tipo makerR :: TimeMachineR -> Maker
data TimeMachineR = TimeMachineR { makeR :: MakeR
                                 , concreteInfoR :: ConcreteInfoR }
                  deriving Show
data MakeR = MakeR { manufacturerR :: ManufacturerR
                   , modelR :: ModelR }
           deriving Show
data ConcreteInfoR = ConcreteInfoR { nameR :: NameR
                                   , travelfeaturesR :: TravelfeaturesR
                                   , priceR :: PriceR }
                  deriving Show
-- type representa sinónimos, que nos ayudan a definir las cosas mejor
type ManufacturerR = String
type ModelR = String
type NameR = String

type TravelfeaturesR = String
type PriceR = Double

timeMachineR1 = TimeMachineR { makeR = MakeR { manufacturerR = "ACME"
                                             , modelR = "T3298-XP Pro" }
                             , concreteInfoR = ConcreteInfoR { nameR = "BetaModel1"
                                                             , travelfeaturesR = "From the Big Bang to the End of Time"
                                                             , priceR = 100000000.5 }
                             }
timeMachineR2 = TimeMachineR { makeR = MakeR { manufacturerR = "ACME"
                                             , modelR = "XATMO-72" }
                             , concreteInfoR = ConcreteInfoR { nameR = "BetaModel2"
                                                             , travelfeaturesR = "1 week in the past or the future from now"
                                                             , priceR = 100 }
                             }

{-
data ClientR = GovOrgR { clientRName :: String }
             | CompanyR { clientRName :: String 
                        , companyId :: Integer
                        , person :: PersonR
                        , duty :: String }
             | IndividualR { person :: PersonR }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       } deriving Show

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p

{ makeR = MakeR { manufacturerR = man
                                                   , modelR = mod
                                                   }
                                   , concreteInfoR { nameR = name
                                                   , travelfeaturesR = travel
                                                   , priceR = _
                                                   }
                                   }

greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet CompanyR    { clientRName = c }                     = "Hello " ++ c
greet GovOrgR     { }                                     = "Welcome"

-- requiere NamedFieldPuns
greet' :: ClientR -> String
greet' IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName
greet' CompanyR    { clientRName }                    = "Hello " ++ clientRName
greet' GovOrgR     { }  
-}

updatePrice :: Double -> TimeMachineR -> TimeMachineR
updatePrice newprice tm@TimeMachineR { makeR, concreteInfoR = ConcreteInfoR { nameR, travelfeaturesR } } =
  tm { concreteInfoR = ConcreteInfoR { nameR, travelfeaturesR, priceR = newprice } }