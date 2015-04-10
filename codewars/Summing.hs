module Summing where

f :: Integer -> Integer -> Integer
f n m = (n `div` m) * (sum [1..m-1]) + sum [1..n `mod` m]