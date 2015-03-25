module JadenCasing where

import Data.Char

toJadenCase :: String -> String
toJadenCase = unwords . (map capitalizeFirst) . words

capitalizeFirst :: String -> String
capitalizeFirst str = (toUpper . head) str : tail str