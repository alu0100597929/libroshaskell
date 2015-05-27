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