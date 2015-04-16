module Codewars.Summing where

import Data.List

minimumSum :: [Integer] -> Int -> Integer
minimumSum xs n
  | n <= 0 || null xs = 0
  | otherwise = (sum . take n) $ sort xs

maximumSum :: [Integer] -> Int -> Integer
maximumSum xs n
  | n <= 0 || null xs = 0
  | otherwise = (sum . take n . reverse) $ sort xs