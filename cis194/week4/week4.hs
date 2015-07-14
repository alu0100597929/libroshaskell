{-# LANGUAGE FlexibleInstances #-}

import Data.Char  ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read  ( readMaybe )
import Data.Monoid hiding ( Product, getProduct ) -- faltaba

-- Recuerda: <> es un sinónimo infijo de mappend
-- así como <$> es un sinónimo infijo de fmap

-- this is not the most efficient!
intInts :: Monoid m => (Integer -> m) -> m   -- interesting ints!
intInts mk_m = go [1..100]   -- [1..100] is the list of numbers from 1 to 100
  where go [] = mempty
        go (n:ns)
          | let div_by_5 = n `mod` 5 == 0
                div_by_7 = n `mod` 7 == 0
          , (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7))
          = mk_m n <> go ns
          | otherwise
          = go ns

intIntsList :: [Integer]
intIntsList = intInts (:[])

data Product a = Product a
instance Num a => Monoid (Product a) where
  mempty                          = Product 1
  mappend (Product x) (Product y) = Product (x * y)

getProduct :: Product a -> a
getProduct (Product x) = x

intIntsProduct :: Integer
intIntsProduct = getProduct $ intInts Product