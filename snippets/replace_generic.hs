import Control.Monad
import Data.List.Split (splitOn)

replace :: String -> String -> String -> String
replace new old str = join new (splitOn old str)