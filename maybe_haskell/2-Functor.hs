import Data.Maybe(fromMaybe)

actuallyFive :: Maybe Int
actuallyFive = Just 5

notReallyFive :: Maybe Int
notReallyFive = Nothing

getValue :: Maybe a -> a
getValue (Just x) = x
getValue Nothing  = error "uh-oh"

-- intentar evitar a toda costa las excepciones

{-
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x
-}

-- pass the buck = pasar la pelota
whenJust :: (a -> b) -> Maybe a -> Maybe b
whenJust f (Just x) = Just (f x)
whenJust _ Nothing = Nothing

-- como se aprecia claramente, esta función es muy parecida
-- (en realidad, igual) a fmap
-- El propósito de los funtores es "elevar" funciones que trabajan
-- con sus datos internos y aplicárselas a funtores

--instance Functor Maybe where
--  fmap f (Just x) = Just (f x)
--  fmap _ Nothing  = Nothing

f :: Int -> Int
f = (+2)

g :: Int -> Int
g = (*3)

h :: Int -> Int
h = f . g

fh :: Maybe Int -> Maybe Int
fh = fmap f . fmap g

{-
findUser :: UserId -> Maybe User
findUser = undefined

userUpperName :: User -> String
userUpperName u = map toUpper (userName u)

maybeName :: Maybe String
maybeName = fmap userUpperName (findUser someId)

template :: Maybe String -> String
template mname = "<span class=\"username\">" ++ name ++ "</span>"
  where
    name = fromMaybe "(no name given)" mname
-}

add :: Int -> (Int -> Int)
add x = \y -> x + y

f :: [Int]
f = map (add 5) [1,2,3]