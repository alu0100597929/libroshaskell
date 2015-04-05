-- Un ejemplo de uso de algo parecido a una mónada Writer

cuadrado :: Float -> (Float, String)
cuadrado x = (x*x, "Elevando " ++ show x ++ " al cuadrado")

cubo :: Float -> (Float, String)
cubo x = (x*x*x, "Elevando " ++ show x ++ " al cubo")

-- (>>=) :: (a -> m b) -> m a -> m b
bind :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind f' (gx,gs) = let (fx,fs) = f' gx 
                    in (fx,gs++fs)

composicion f g = bind f . g