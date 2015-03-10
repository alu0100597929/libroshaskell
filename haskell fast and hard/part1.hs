{-main = do
    print "What is your name?"
    name <- getLine
    print ("Hello " ++ name ++ "!")
    print "In what city are you now?"
    ciudad <- getLine
    print (ciudad ++ " is a very beautiful city!")-}

--averiguando los tipos
{-g x y = x^2 - y^2 + x - y

f x y = x*x + y*y

main = print (f 2.3 4.2)-}

{-f :: Num a => a -> a -> a
f x y = x*x + y*y

g :: Num a => a -> a
g = f 3

--lambdas
h = \y -> 3*3 + y*y-}

f x = x
h x = "Hello"
p a b c x = a*x*x + b*x + c