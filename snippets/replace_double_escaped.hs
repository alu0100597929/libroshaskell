-- input:  "(case (+ 5 5) ((4 9 1) 'd64)\\n((1 2) 'pepito)\\n((10) '789.456)"
-- output: "(case (+ 5 5) ((4 9 1) 'd64)\n((1 2) 'pepito)\n((10) '789.456)"

import Data.List (intersperse)
import Data.List.Split (splitOn)

foo :: String -> String
foo s = read $ "\"" ++ s ++ "\""

-- replace old new str = intersperse new (splitOn old str)