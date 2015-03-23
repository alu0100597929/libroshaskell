import Data.Ratio --Para usar fracciones. Usa el operador % como la raya de fracción

multiplicarFracciones = (11 % 15) * (5 % 3)

--Para texto, es mejor usar Data.Text
--Para un flujo de caracteres ASCII, es mejor usar Data.ByteString

--ejercicio
--selectWin1 :: (a, b) -> b
{-selectWin1 = snd
main = do
  print $ selectWin1 (1,"win") -- should return "win"-}

{-selectWin2 = fst . fst
main = do
  print $ selectWin2 (("win","no"),"not this one")-}

{-selectWin3 = fst . snd . snd
main = do
  print $ selectWin3 (1,("no",("win","almost")))-}

{-
x :: Int            ⇔ x is of type Int
x :: a              ⇔ x can be of any type
x :: Num a => a     ⇔ x can be any type a
                      such that a belongs to Num type class
f :: a -> b         ⇔ f is a function from a to b
f :: a -> b -> c    ⇔ f is a function from a to (b→c)
f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
-}

square :: Num a => a -> a
square x = x^2

square' x = (^) x 2

square'' x = (^2) x

square''' = (^2)

absolute :: (Ord a, Num a) => a -> a
absolute x = if x >= 0 then x else -x

absolute' x
    | x >= 0 = x
    | otherwise = -x

{-main = do
      print $ square 10
      print $ square' 10
      print $ square'' 10
      print $ square''' 10
      print $ absolute 10
      print $ absolute (-10)
      print $ absolute' 10
      print $ absolute' (-10)-}

{-ejercicio, modificar el código para usar notación prefija
f x y = x*x + y*y
main = print $ f 2 4-}

f x y = (+) ((*) x x) $ (*) y y
main = print $ f 2 4