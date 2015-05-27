module Chapter2.Section2.DataTypes where

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