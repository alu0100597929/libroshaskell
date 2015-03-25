import Data.List (take, iterate)

seqlist :: Int -> Int -> Int -> [Int]
seqlist first c l = take l $ iterate (+c) first

-- seqlist :: Int -> Int -> Int -> [Int]
-- seqlist first c l = take l [first, first + c ..]