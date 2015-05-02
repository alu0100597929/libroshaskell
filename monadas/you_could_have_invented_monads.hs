import Data.Complex
import System.Random
import Control.Monad.Writer

cuadrado :: Float -> Float
cuadrado = (^2)

cubo :: Float -> Float
cubo = (^3)

cuadradoInfo :: Float -> (Float, String)
cuadradoInfo x = (x^2, "se ha llamado a cuadrado con " ++ show x)

cuboInfo :: Float -> (Float, String)
cuboInfo x = (x^3, "se ha llamado a cubo con " ++ show x)

-- bindW significa bind para Writer
-- bindW f' :: (Float,String) -> (Float,String)

-- por muchos paréntesis que veamos, da igual, la currificación se resuelve
-- como siempre, bind en este caso recibe dos cosas para dar una última,
-- un resultado (Float, String)
bindW :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bindW f (num,str) = let (num', str') = f num in (num', str++str')

unitW :: Float -> (Float, String)
unitW x = (x,"")

{-
*Main> (bind cuadradoInfo . unit) 5
(25.0,"se ha llamado a cuadrado con 5.0")
*Main> (bind cuboInfo . bind cuadradoInfo . unit) 5
(15625.0,"se ha llamado a cuadrado con 5.0se ha llamado a cubo con 25.0")
-}

-- composición de funciones 
com f g = bindW f . g

{-
*Main> (com cuadradoInfo cuboInfo) 3
(729.0,"se ha llamado a cubo con 3.0se ha llamado a cuadrado con 27.0")
-}

-- Mediante unit conseguimos hacer un lift, es decir, podemos "elevar"
-- cualquier función normal a una con un contexto monádico. La
-- "información oculta" o efecto lateral es añadir la cadena vacía, que
-- en nuestra mónada es una especie de elemento neutro
-- lift f x = (f x,"")
liftW f = unitW . f

--sqrt',cbrt' :: Complex Double -> [Complex Double]
sqrt' :: Complex Double -> [Complex Double]
sqrt' x = let raiz = sqrt x in
            [raiz, negate raiz]

-- (**) es exponenciación fraccionaria
cbrt' :: Complex Double -> [Complex Double]
cbrt' x = let raiz = x ** (1/3) in
            [raiz, negate raiz]

bindL :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
bindL f xs = concat $ map f xs

sixthroot x = bindL cbrt' $ sqrt' x

unitL x = [x]

liftL f = unitL . f

-- función estándar randomizada
-- a -> StdGen -> (b,StdGen)

-- bind :: (a -> M b) -> M a -> M b
bindR :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
bindR f x seed = let (x', seed') = x seed
                 in f x' seed'

unitR :: a -> (StdGen -> (a,StdGen))
-- unitR x g = (x, g)
unitR = (,)

liftR f = unitR . f

-- Iluminación
type Debuggable a = (a,String)
type Multivalued a = [a]
type Randomised a = StdGen -> (a,StdGen)

manyOps' :: Writer String Integer
manyOps' = return 7 >>= (\x -> writer (x+1, "inc."))
              >>= (\x -> writer (2*x, "double."))
          >>= (\x -> writer (x-1, "dec"))

{-
manyOps :: Writer String Integer
manyOps = do
            x <- return 7
            x <- writer (x+1, "inc.")
            x <- writer (2*x, "double.")
            writer (x-1, "dec")
-}

manyOps :: Writer String Integer
manyOps = do
            let x = 7
            y <- writer (x+1,"inc\n")
            z <- writer (2*y,"double\n")
            writer (z-1,"dec\n")

raizSexta = return 64 >>= (\x -> sqrt' x) >>= (\y -> cbrt' y)

raizSexta' = do
               let x = 64
               y <- sqrt' x
               return $ cbrt' y -- podemos omitir el return si queremos

addDigit n g = let (a,g') = random g in (n + a `mod` 10,g')

shift = liftR (*10)

test :: Integer -> StdGen ->  (Integer,StdGen)
test = bindR addDigit . bindR shift . addDigit

g = mkStdGen 666

main = print $ test 0 g

