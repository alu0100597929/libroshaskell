--import Prelude hiding (show, (==))
import qualified Data.Map as Map

--tipo Either, en este caso sería LockerState el que lo usa
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

--esto nos va a dar la información de por qué no se obtuvo el código en caso de error, si fue por no existir o
--porque ya estaba cogido
lockerLookup :: Int -> LockerMap -> Either String Code --puede dar a) error, String ó b) código
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber
                          ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken, "ZD39I"))
  ,(101,(Free, "JAH3I"))
  ,(103,(Free, "IQSA9"))
  ,(105,(Free, "QOTSA"))
  ,(109,(Taken, "893JJ"))
  ,(110,(Taken, "99292"))
  ]

{-
ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"
-}

--aquí definimos un nuevo operador :-:
--infixr n significa asociativo a la derecha
--infixl n significa asociativo a la izquierda
--la n establece la prioridad, es decir, cuanto mayor sea, antes se debe hacer su operación

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

{-
ghci> 3 :-: 4 :-: 5 :-: Empty
3 :-: (4 :-: (5 :-: Empty))
ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> 100 :-: a
100 :-: (3 :-: (4 :-: (5 :-: Empty)))

--definición de ++ para listas estándar de Haskell
infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]
 ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

--Árboles
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right --Esto puede confundir, pero en realidad se está devolviendo un árbol, el mismo que había ya que no puede haber elementos repetidos
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

deListaAArbol :: (Ord a) => [a] -> Tree a
deListaAArbol = foldr (treeInsert) EmptyTree

--a es una variable de tipos, no tiene xq llamarse a, ni que ser una sola letra
--lo único que se debe cumplir es que todas las letras sean minúsculas.
--definición mínima completa: aquí lo que ocurre es que hemos definido == y /= como
--el not de su función contraria. De este modo, sólo tenemos que implementar la función
--más fácil de implementar (en este caso ==) y ta estaría todo terminado

{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
-}

--no derivamos a ninguna clase de tipos porque implementaremos nosotros todo
data TrafficLight = Red | Yellow | Green

--class: define una nueva clase de tipos
--instante: hacemos a nuestra clase instancia de una clase de tipos
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False --suele ser llamada catchall

--cuidado con la indentación de este y el de arriba, si no están indentados da mil errores de compilación
--creando nosotros la instancia, podemos mostrar la cadena que queramos
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

{-
--esto es una subclase, simplemente es decir que para ser un número primero debe ser un tipo para el cual
--se pueda comprobar si es igual o distinto, es decir, añadir una class constraint
class (Eq a) => Num a where
-}

--un constructor de tipos produce un tipo concreto

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult

{-
--functores: reciben una función (a -> b) de un tipo a otro, y un functor aplicado al primer tipo (a)
--devolviendo un functor aplicado al segundo tipo
class Functor f where
  fmap :: (a -> b) -> f a -> f b

map :: (a -> b) -> [a] -> [b]
--por tanto map es un fmap que sólo actúa sobre listas

--aquí [] es un constructor de tipos que recibe un tipo y puede producir tipos como [Int], [Char], etc.
instance Functor [] where
fmap = map

Functor wants a type constructor that takes one type, and not a concrete type.
Por eso usa Maybe en vez de Maybe a.

If you mentally replace the fs with Maybes in fmap :: (a -> b) -> f a -> f b, fmap acts like
a (a -> b) -> Maybe a -> Maybe b for this particular type, which looks okay. But if you
replace f with (Maybe m), then it would seem to act like a (a -> b) -> Maybe m a -> Maybe m b,
which doesn’t make sense, because Maybe takes just one type parameter.

ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
Just "Something serious. HEY GUYS IM INSIDE THE JUST"
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
Nothing
ghci> fmap (*2) (Just 200)
Just 400
ghci> fmap (*2) Nothing
Nothing

Las cosas que pueden ser instancias de Functor son aquellas que puedan ser mapeadas, es decir,
se les pueda aplicar map. Además, debemos poder pensar en ellas como cajas, que pueden tener dentro
otras cajas, valores, o nada.

Ejemplos: listas, árboles...
-}

--hay que tener cuidado con qué función mapeamos sobre los árboles de búsqueda binaria, porque
--si por ejemplo mapeamos negate, perderemos la propiedad de búsqueda binaria
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

{-
Either recibe dos parámetros, pero Functor solo quiere que reciba uno. ¿Qué hacemos? Pues aplicar
parcialmente un parámetro a su constructor:

instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

(b -> c) -> Either a b -> Either a c

-}

--TODO
--instance Functor (Map k) where
--fmap f =

