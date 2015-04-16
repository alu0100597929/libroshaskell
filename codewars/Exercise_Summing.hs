module Codewars.Summing where

minimumSum :: [Integer] -> Int -> Integer
minimumSum xs n
  | n <= 0 || null xs = 0
  | otherwise = sum $ take n xs

maximumSum :: [Integer] -> Int -> Integer
maximumSum xs = minimumSum (reverse xs)