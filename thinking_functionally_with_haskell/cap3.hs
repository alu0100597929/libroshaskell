-- función incorrecta
floorMalHecha :: Float -> Integer
floorMalHecha = read . takeWhile (/= '.') . show

until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x = if p x then x else until' p f (f x)

floorLenta :: Float -> Integer
floorLenta x = if x < 0
                 then until (`leq` x) (subtract 1) (-1)
                 else until (x `lt`) (+1) 1 - 1
  where x `lt` n = x < fromInteger n
        x `leq` n = fromInteger x <= n