module Dictionaries where

import Data.List

sortDict :: Ord v => [(k,v)] -> [(k,v)]
sortDict = sortBy sort2Tuples

-- al poner al revés GT y LT en cada caso, se ordenan de mayor a menor
sort2Tuples :: Ord v => (k,v) -> (k,v) -> Ordering
sort2Tuples (_,a) (_,b)
  | a == b = EQ
  | a <  b = GT
  | otherwise = LT