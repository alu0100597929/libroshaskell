module Chapter2.Section2.SimpleFunctions where

-- Exercise 2.1

--literal1 = [] ('a' : 'b' : 'c' : []) ('d':'e':[])

emptyOrFirstEmpty :: [[a]] -> Bool
emptyOrFirstEmpty []     = True
emptyOrFirstEmpty ([]:_) = True
emptyOrFirstEmpty _      = False

onlyOne :: [a] -> Bool
onlyOne [_] = True
onlyOne _   = False

myConcat ::[a] -> [a] -> [a]
myConcat xs []     = xs
myConcat xs (y:ys) = myConcat (xs ++ [y]) ys

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty xs = if not (null xs)
                    then head xs
                    else "empty"

lst1 +++ lst2 = if null lst1 {- check emptyness -}
                  then lst2 -- base case
                  else (head lst1) : (tail lst1 +++ lst2)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]

maxmin :: (Ord a) => [a] -> (a,a)
maxmin []     = error "no hay máximo ni mínimo en una lista vacía"
maxmin [x]    = (x,x) --las listas con un sólo elemento tienen ese elemento por máximo y mínimo
maxmin (x:xs) = (if t_max > x then t_max else x,
                 if t_min < x then t_min else x)
  where
    t_max = fst t
    t_min = snd t
    t = maxmin xs