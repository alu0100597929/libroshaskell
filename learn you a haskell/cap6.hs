import Data.Char --para el cifrado César, para show
import Data.List
import qualified Data.Map as Map
--import Data.List (nub, sort)   --> sólo importa nub y sort
--import Data.List hiding (nub)  --> las coge todas menos nub
{-import qualified Data.Map as M --> evita solapamientos con las funciones del Prelude, por ejemplo.
                                     para llamar al filter de Map sería: M.filter
                                     importante no usar espacios, ya que si no podría haber ambiguedad con la composición-}

--nub devuelve una lista con los elementos distintos dentro de una lista, es decir, quita los repetidos
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

{-
--words separa una String por palabras, quedando cada palabra en una String, devuelve una lista de String
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]

--agrupa en listas aquellos elementos adyacentes que son iguales
ghci> group ["boom","bip","bip","boom","boom"]
[["boom"],["bip","bip"],["boom","boom"]]

--ordena tanto números como String, de menor a mayor
ghci> sort [5,4,3,7,2,1]
[1,2,3,4,5,7]

ghci> sort ["boom","bip","bip","boom","boom"]
["bip","bip","boom","boom","boom"]
-}

--queremos contar todas las veces que aparece cada palabra en una String
cuentaPalabras :: String -> [(String,Int)]
cuentaPalabras = map (\ws -> (head ws, length ws)) . group . sort . words

--recuerda: any devuelve verdadero siempre que al menos un elemento cumpla la condición
--isPrefixOf devuelve verdadero si la primera lista que le pasamos es prefijo de la segunda lista que le pasamos 
--esta función ya existe en Data.List como isInfixOf
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

{-
ghci> ord 'a'
97

ghci> chr 97
'a'

ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]
-}

--cifrado César
cifrar :: String -> Int -> String
cifrar s offset = map (\x -> chr $ ord x + offset) s

--cowboy de la composición xDDDDDDDDDD
cifrar' :: String -> Int -> String
cifrar' s offset = map (chr . (+ offset) . ord) s

descifrar :: String -> Int -> String
descifrar s offset = cifrar s $ negate offset

--buen truco!! show pasa el int a cadena, luego mapeamos una función que pasa dígitos a int y sumamos
sumaCifras :: Int -> Int
sumaCifras = sum . map digitToInt . show

--es mejor no usar map antes de find, al menos para casos fáciles, "mapear" en el propio find sin usar map
sumaCifrasIgual :: Int -> Maybe Int
sumaCifrasIgual n = find (\x -> sumaCifras x == n) [1..]

{-
phoneBook =
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]
-}

--función que va bien si existe la entrada, si no, error en tiempo de ejecución
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

--nos damos cuenta de que éste es el patrón clásico de fold
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs)
  | key == k = Just v
  | otherwise = findKey' key xs

--evitar usar head cuando no estemos seguros de que la lista tendrá algo
--mejor usar un foldr que la recursión explícita, se ve más claro de primeras
findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

{-
ghci> Map.fromList [(3,"shoes"),(4,"trees"),(9,"bees")]
fromList [(3,"shoes"),(4,"trees"),(9,"bees")]

ghci> Map.fromList [("kima","greggs"),("jimmy","mcnulty"),("jay","landsman")]
fromList [("jay","landsman"),("jimmy","mcnulty"),("kima","greggs")]

--elimina duplicados
ghci> Map.fromList [("MS",1),("MS",2),("MS",3)]
fromList [("MS",3)]

Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
es decir, necesita que las claves sean ordenables para buscarlas más rápido, y devuelve un mapa de
claves a valores
-}

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]

{-
ghci> :t Map.lookup
Map.lookup :: (Ord k) => k -> Map.Map k a -> Maybe a
ghci> Map.lookup "betty" phoneBook
Just "555-2938"
ghci> Map.lookup "wendy" phoneBook
Just "939-8282"
ghci> Map.lookup "grace" phoneBook
Nothing
-}

{-
ghci> :t Map.insert
Map.insert :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
ghci> Map.lookup "grace" phoneBook
Nothing
ghci> let newBook = Map.insert "grace" "341-9021" phoneBook
ghci> Map.lookup "grace" newBook
Just "341-9021"

ghci> :t Map.size
Map.size :: Map.Map k a -> Int
ghci> Map.size phoneBook
6
ghci> Map.size newBook
7
-}

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

{-
ghci> let intBook = Map.map string2digits phoneBook
ghci> :t intBook
intBook :: Map.Map String [Int]
ghci> Map.lookup "betty" intBook
Just [5,5,5,2,9,3,8]
-}

--ahora vamos a modificar el comportamiento del mapa para que aguante varios números por persona
{-
phoneBook =
  [("betty", "555-2938")
  ,("betty", "342-2492")
  ,("bonnie", "452-2928")
  ,("patsy", "493-2928")
  ,("patsy", "943-2929")
  ,("patsy", "827-9162")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ,("penny", "555-2111")
  ]
-}

--cuando encuentra una clave duplicada llama a la función add (o a la que definamos) la cual concatena
--los números separados por comas, sean cuantos sean
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2.

{-
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"
-}

--convierte cada valor String en una lista singleton (de un solo elemento de tipo String)
--y luego aplica el operador (++) cuando una clave se repite, en este caso se devuelve una lista
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

{-
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
["827-9162","943-2929","493-2928"]
-}

{-
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,100),(3,29),(4,22)]
-}

{-
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
fromList [(2,108),(3,62),(4,37)]
-}

