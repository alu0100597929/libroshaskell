module TheShellGame where

newPosition :: Int -> (Int, Int) -> Int
newPosition p (x,y) = if p == x
                         then y
                         else if p == y
                                 then x
                                 else p

findTheBall :: Int -> [(Int, Int)] -> Int
findTheBall p = foldl newPosition p