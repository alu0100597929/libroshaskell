--La mónada lista

{-
instance Monad [] where
  -- return :: a -> [a]
  return x = [x]

  -- (>>=)  :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concat (map f xs)
-}

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)

pairs' :: [a] -> [b] -> [(a,b)]
pairs' xs ys = xs >>= 
                 (\x -> ys >>=
                   (\y -> return (x,y)))

--expandiremos la definición de pairs para ver el despliegue de >>= para la mónada lista
--xs >>= (\x -> ys >>= (\y -> return (x,y))) -- => por la definición de bind
--concat (map (\x -> ys >>= (\y -> return (x,y)))) xs -- => por la definición de bind
--concat (map (\x -> concat (map (\y -> return (x,y)) ys)) xs)

pairs'' xs ys = [(x,y) | x <- xs, y <- ys]