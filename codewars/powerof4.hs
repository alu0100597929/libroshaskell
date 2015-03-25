{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

isInteger :: (Typeable a) => a -> Bool
isInteger n = typeOf n == typeOf 1

{-isPowerOf4 :: a -> Bool
isPowerOf4 x = if isInteger x
                   then raizcuarta (fromIntegral x)
                   else False
-}

raizcuarta :: (Floating a) => a -> Bool
raizcuarta y = if typeOf (sqrt (sqrt y)) == typeOf (1::Integer)
                       then True
                       else False