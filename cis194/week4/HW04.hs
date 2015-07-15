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

mostrarPolinomio :: (Num a, Eq a, Show a) => Poly a -> String
mostrarPolinomio (P xs) = mostrarPoli (P xs) 0

mostrarGrado :: Int -> String
mostrarGrado 0  = "1"
mostrarGrado 1  = "x"
mostrarGrado x  = "x^" ++ show x

mostrarTermino :: (Num a, Eq a, Show a) => a -> Int -> String
mostrarTermino coeficiente grado = case coeficiente of
                                     -1 -> "-" ++ mostrarGrado grado
                                     0  -> ""
                                     1  -> mostrarGrado grado
                                     c  -> if grado == 0 then show c else show c ++ mostrarGrado grado

mostrarPoli :: (Num a, Eq a, Show a) => Poly a -> Int -> String
mostrarPoli (P []) n     = ""
mostrarPoli (P [x]) n    = mostrarTermino x n 
mostrarPoli (P (x:xs)) n = mostrarPoli (P xs) (n + 1) ++ case mostrarTermino x n of
                                                           ""  -> ""
                                                           cad -> " + " ++ cad

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show = mostrarPolinomio

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (sumarListasPorPosiciones xs ys)

sumarListasPorPosiciones :: Num a => [a] -> [a] -> [a]
sumarListasPorPosiciones xs ys = let mayor = if (length xs > length ys) then xs else ys
                                     parteComun = zipWith (+) xs ys
                                 in parteComun ++ (drop (length parteComun) mayor)

producto :: Num a => Poly a -> Poly a -> Poly a
producto (P xs) (P ys) = foldl1 plus (productosPorTermino (P xs) (P ys) 0)

productosPorTermino :: Num a => Poly a -> Poly a -> Int -> [Poly a]
productosPorTermino (P []) (P ys) _     = []
productosPorTermino (P xs) (P []) _     = []
productosPorTermino (P xs) (P ys) n = P ((take n $ repeat 0) ++ map ((last xs)*) ys) : productosPorTermino (P (tail xs)) (P ys) (n + 1)

{-
show (P [1,0,0,2]) == "2x^3 + 1"
show (P [0,-1,2]) == "2x^2 + -x"
-}

negate :: Num a => Poly a -> Poly a
negate (P xs) = P (map Prelude.negate xs)

fromIntegerPoli :: Num a => Integer -> Poly a
fromIntegerPoli n = P [(fromInteger n)]

applyP :: Num a => Poly a -> a -> a
applyP (P xs) = applyP' $ P (reverse xs)

applyP' :: Num a => Poly a -> a -> a
applyP' (P []) _ = 0
applyP' (P toda@(x:xs)) n = let largo = length toda
                            in x*n^(largo - 1) + applyP' (P xs) n

class Num a => Differentiable a where
  deriv :: a -> a
  nderiv :: Int -> a -> a

multiplicarPorSuIndice :: Num a => [a] -> [a]
multiplicarPorSuIndice (xs) = zipWith (*) xs $ map fromInteger [0..toInteger (length xs) -1]

derivarPoli :: Num a => Poly a -> Poly a
derivarPoli (P xs) = let (y:ys) = multiplicarPorSuIndice xs
                     in P ys