newtype Poly a = P [a]

x :: Num a => Poly a
x = P [0,1]

todaCeros :: (Num a, Eq a) => [a] -> Bool
todaCeros = all (== 0)

instance (Num a, Eq a) => Eq (Poly a) where
  (P xs) == (P ys) = if (todaCeros xs && todaCeros ys || xs == ys)
                             then True
                             else False

equalsToZero :: (Num a, Eq a) => Poly a -> Bool
equalsToZero (P xs) = todaCeros xs  

{-
*Main> P [1,2,3] == P [1,2,3]
True
*Main> P [1,2] == P [1,2,3]
False
*Main> P [1,2] /= P [1,2,3]
True
-}

