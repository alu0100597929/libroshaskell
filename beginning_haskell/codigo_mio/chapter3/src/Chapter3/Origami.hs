module Chapter3.Origami where

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold p = foldr (\x acc -> if p x then x:acc else acc) []

