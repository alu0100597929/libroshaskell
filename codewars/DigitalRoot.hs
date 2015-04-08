module DigitalRoot where

toList 0 = []
toList n = toList (n `div` 10) ++ [n `mod` 10]

sumCiphers :: Integral a => a -> a
sumCiphers n = sum $ toList n

digitalRoot :: Integral a => a -> a
digitalRoot n = if n < 10
                   then n
                   else digitalRoot $ sumCiphers n

-- fórmula pro
digitalRoot' :: Integral a => a -> a
digitalRoot' n = 1 + (n - 1) `mod` 9