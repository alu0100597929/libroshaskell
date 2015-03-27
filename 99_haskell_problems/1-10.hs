import Data.List

myLast = last

myButLast = last . init

elementAt :: Int -> [a] -> a
elementAt n xs = xs !! pred n

myLength :: [a] -> Int
myLength = foldr (\x n -> n + 1) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []


data NestedList a = Elem a | List [NestedList a]

{-flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []-}

flatten :: NestedList a -> [a]
flatten (Elem x) = return x
flatten (List x) = x >>= flatten

-- problem 8
quitarDuplicados :: (Eq a) => [a] -> [a]
quitarDuplicados [] = []
quitarDuplicados [x] = [x]
quitarDuplicados (x:y:ys) = if x == y
                               then quitarDuplicados (y:ys)
                               else x : (quitarDuplicados (y:ys))

pack :: (Eq a) => [a] -> [[a]]
pack = group