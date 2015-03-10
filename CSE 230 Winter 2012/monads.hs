-- https://cseweb.ucsd.edu/classes/wi12/cse230-a/lectures/monads.html

import Prelude hiding ((>>=))

inc :: [Int] -> [Int]
inc []     =  []
inc (n:ns) =  n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     =  []
sqr (n:ns) =  n^2 : sqr ns

data Expr1 = Val1 Int | Div1 Expr1 Expr1

eval1 :: Expr1 -> Int
eval1 (Val1 n)   =  n
eval1 (Div1 x y) =  eval1 x `div` eval1 y

safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m)

eval1' :: Expr1 -> Maybe Int
eval1' (Val1 n)   =  Just n
eval1' (Div1 x y) =  case eval1' x of 
                          Nothing -> Nothing
                          Just n1 -> case eval1' y of
                                          Nothing -> Nothing
                                          Just n2 -> n1 `safeDiv` n2

data Expr = Val Int | Div Expr Expr

{-
safediv :: Int -> Int -> Maybe Int
safediv n m =  if m == 0 
                 then Nothing
                 else Just (n `div` m)
-}

{-
eval :: Expr -> Maybe Int
eval (Val n)   =  Just n
eval (Div x y) =  case eval x of
                       Nothing -> Nothing
                       Just n  -> case eval y of
                                       Nothing -> Nothing
                                       Just m  -> safediv n m
-}

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply _ Nothing  = Nothing
apply f (Just x) = f x

{-
eval (Val n)   = Just n
eval (Div x y) = apply f (eval x `seqn` eval y)
                   where f (n, m) = safediv n m
-}

{-
(>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f =  case m of
             Nothing -> Nothing
             Just x  -> f x
-}

safediv :: Int -> Int -> Maybe Int
safediv n m =  if m == 0 
                 then Nothing
                 else Just (n `div` m)

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _ = Nothing
(Just x) >>= f = f x

--data Expr = Val Int | Div Expr Expr

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))

--lo mismo de arriba, pero usando notación do
evalDo :: Expr -> Maybe Int
evalDo (Val n)   = Just n
evalDo (Div x y) = do n <- evalDo x
                      m <- evalDo y
                      safediv n m

seqn :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing   _        =  Nothing
seqn _         Nothing  =  Nothing
seqn (Just x)  (Just y) =  Just (x, y)

--Ejercicios: Redefine seqn x y and eval (Op x y z) using the do notation.

--1º pasamos a cases
seqnCases :: Maybe a -> Maybe b -> Maybe (a,b)
seqnCases a b = case a of
                  Nothing -> Nothing
                  Just x -> case b of
                              Nothing -> Nothing
                              Just y -> Just (x,y)

seqnDo :: Maybe a -> Maybe b -> Maybe (a,b)
seqnDo a b = do x <- a
                y <- b
                Just (x,y)

seqnBind :: Maybe a -> Maybe b -> Maybe (a,b)
seqnBind a b = a >>= \x -> b >>= \y -> Just (x,y)

{-
--definición original de eval
eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _ = Nothing
(Just x) >>= f = f x
-}

--expandimos el operador bind usando cases
evalCases :: Expr -> Maybe Int
evalCases (Val n)   = Just n
evalCases (Div x y) = case evalCases x of
                        Nothing -> Nothing
                        (Just n) -> case evalCases y of
                                      Nothing -> Nothing
                                      (Just m) -> safediv n m