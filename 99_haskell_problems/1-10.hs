import Data.List

myLast = last

myButLast = last . init

elementAt :: Int -> [a] -> a
elementAt n xs = xs !! pred n

myLength :: [a] -> Int
myLength = foldr (\x n -> n + 1) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

{-TODO
data NestedList a = Elem a | List [NestedList a]

flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten xs
-}

quitarDuplicados :: (Eq a) => [a] -> [a]
quitarDuplicados [] = []
quitarDuplicados [x] = [x]
quitarDuplicados (x:y:ys) = if x == y
                               then quitarDuplicados (y:ys)
                               else x : (quitarDuplicados (y:ys))