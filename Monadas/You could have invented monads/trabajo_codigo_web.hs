-- http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

import Data.Complex
import System.Random

-- f,g :: Float -> Float
-- f',g' :: Float -> (Float,String)

-- let (y,s) = g' x
-- (z,t) = f' y in (z,s++t)

cuadrado x = (x*x, "Elevado al cuadrado. ")

cubo x = (x * x * x, "Elevado al cubo. ")

-- queremos f' . g'
bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (gx,gs) = let (fx,fs) = f' gx 
                    in (fx,gs++fs)

composicion f g = bind f . g 

unit x = (x,"")

--lift f x = (f x,"")

lift f = unit . f

-- sqrt',cbrt' :: Complex Float -> [Complex Float]
bindC :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
bindC f x = concat (map f x)

unitC x = [x]

composicionC :: (Complex Double -> [Complex Double]) -> (Complex Double -> [Complex Double])
             -> (Complex Double -> [Complex Double])
composicionC f g = bindC f . g

liftC f = unitC . f

bindR :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
bindR f x seed = let (x', seed') = x seed
                   in f x' seed'

unitR :: a -> (StdGen -> (a,StdGen))
unitR = (,)

composicionR f g = bindR f . g

liftR f = unitR . f

type Debuggable a = (a,String)
type Multivalued a = [a]
type Randomised a = StdGen -> (a,StdGen)