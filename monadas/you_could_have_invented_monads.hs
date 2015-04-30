cuadrado :: Float -> Float
cuadrado = (^2)

cubo :: Float -> Float
cubo = (^3)

cuadradoInfo :: Float -> (Float, String)
cuadradoInfo x = (x^2, "se ha llamado a cuadrado con " ++ show x)

cuboInfo :: Float -> (Float, String)
cuboInfo x = (x^3, "se ha llamado a cubo con " ++ show x)

-- bind f' :: (Float,String) -> (Float,String)

-- por muchos paréntesis que veamos, da igual, la currificación se resuelve
-- como siempre, bind en este caso recibe dos cosas para dar una última,
-- un resultado (Float, String)
bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f (num,str) = let (num', str') = f num in (num', str++str')

unit :: Float -> (Float, String)
unit x = (x,"")

{-
*Main> (bind cuadradoInfo . unit) 5
(25.0,"se ha llamado a cuadrado con 5.0")
*Main> (bind cuboInfo . bind cuadradoInfo . unit) 5
(15625.0,"se ha llamado a cuadrado con 5.0se ha llamado a cubo con 25.0")
-}

-- composición de funciones 
com f g = bind f . g

{-
*Main> (com cuadradoInfo cuboInfo) 3
(729.0,"se ha llamado a cubo con 3.0se ha llamado a cuadrado con 27.0")
-}

-- Mediante unit conseguimos hacer un lift, es decir, podemos "elevar"
-- cualquier función normal a una con un contexto monádico. La
-- "información oculta" o efecto lateral es añadir la cadena vacía, que
-- en nuestra mónada es una especie de elemento neutro
-- lift f x = (f x,"")
lift f = unit . f