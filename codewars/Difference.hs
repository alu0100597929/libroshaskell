module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference [] _ = []
difference xs [] = xs
difference (x:xs) ys = if x `elem` ys
                         then difference xs ys
                         else x : difference xs ys

difference' :: Eq a => [a] -> [a] -> [a]
difference' a b = filter (`notElem` b) a