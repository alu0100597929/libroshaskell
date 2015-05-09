import Data.List.Split (splitOn)
import Data.List (intersperse)
import Data.Char

-- TODO

replace :: String -> String -> String -> [String]
replace current next [] = []
replace current next xs = intersperse [chr 92] $ splitOn current xs