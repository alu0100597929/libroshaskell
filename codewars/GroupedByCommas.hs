-- module Codewars.Commas where
module Commas where

import Data.Char

-- 123456789

intToStr :: Int -> String
intToStr n
  | n <= 0 = []
  | otherwise = intToStr (n `div` 10) ++ [intToDigit (n `mod` 10)]

groupByCommas n = reverse $ groupByCommas' n

groupByCommas' :: Int -> String
groupByCommas' 0 = []
groupByCommas' n = (take 3 cad) ++ if next > 0
                                     then "," ++ groupByCommas' next
                                     else ""
  where
    next = n `div` 1000
    cad = reverse $ intToStr n

{-
import Data.List
import Data.List.Split

groupByCommas :: Int -> String
groupByCommas = reverse . intercalate "," . chunksOf 3 . reverse . show
-}