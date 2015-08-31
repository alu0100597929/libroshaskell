import Data.List

myLast :: [a] -> a
myLast [] = error "lista vacía"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "lista vacía"
myButLast [x] = error "lista de un solo elemento"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "la lista vacía no contiene elementos"
elementAt (x:xs) n = if n == 0
                       then x
                       else elementAt xs (n - 1)

myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys) = if (x == y)
                      then compress (y:ys)
                      else x : compress (y:ys)

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] accL = []
pack' [x] accL = [x:accL]
pack' (x:xs) accL = if (null accL)
                     then pack' xs [x]
                     else if (head accL == x)
                            then pack' xs (x:accL)
                            else accL : pack' (x:xs) []

pack xs = pack' xs []

encode' :: Eq a => [a] -> [a] -> [(Int, a)]
encode' [] accL = []
encode' [x] accL = [(length accL + 1, x)]
encode' (x:xs) accL = if (null accL)
                        then encode' xs [x]
                        else if (head accL == x)
                               then encode' xs (x:accL)
                               else (length accL, head accL) : encode' (x:xs) []

encode xs = encode' xs []