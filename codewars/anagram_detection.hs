import Data.List
import Data.Char

isAnagramOf :: String -> String -> Bool
isAnagramOf test original = (sort mayus_original) == (sort mayus_test)
  where
    mayus_original = map toUpper original
    mayus_test = map toUpper test