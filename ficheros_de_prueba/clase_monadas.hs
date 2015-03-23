cuadrado :: Float -> (Float, String)
cuadrado x = (x*x, "Se está elevando al cuadrado")

cubo :: Float -> (Float, String)
cubo x = (x*x*x, "Se está elevando al cubo")

-- (>>=) :: (a -> m b) -> m a -> m b
bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (gx,gs) = let (fx,fs) = f' gx 
                    in (fx,gs++fs)

composicion f g = bind f . g