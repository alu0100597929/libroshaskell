module BreakingChocolate where

breakChocolate :: Int -> Int -> Int
breakChocolate 1 m = m - 1
breakChocolate n 1 = n - 1
breakChocolate n m = if n <= 0 || m <= 0
                         then 0
                         else (m-1) * (n-1)