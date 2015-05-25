{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches cod1 cod2 = (sum . map (\x -> if x then 1 else 0) $ zipWith (==) cod1 cod2) :: Int

-- Exercise 2 -----------------------------------------

-- helper que cuenta el número de apariciones de un color en una lista de colores
sumApariciones :: Peg -> Code -> Int
sumApariciones col cod = length $ filter (==col) cod

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\col -> sumApariciones col code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches cod1 cod2 = (sum $ zipWith min (countColors cod1) (countColors cod2)) :: Int

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
  where
    nonexact = (matches secret guess) - exact
    exact = exactMatches secret guess

--getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] ==
--Move [Red,Orange,Orange,Blue] 1 2

-- Exercise 4 -----------------------------------------

pair_matches :: Move -> (Int,Int)
pair_matches (Move _ e i) = (e,i) 

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e i) secret = let move_generated = getMove secret guess
                                       in pair_matches move_generated == (e,i)
-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes mov xs = filter (isConsistent mov) xs

-- Exercise 6 -----------------------------------------

allColors :: [[Peg]]
allColors = map (\c -> [c]) colors

-- from a list of all possible n-1 length codes, produce a list of all n-length codes
appendWithAll :: [Code] -> [Code] -- Code = [Peg] ---> [Code] = [[Peg]]
appendWithAll xs = concatMap (\col_xs -> map (\col -> col_xs ++ [col]) colors) xs

-- recibe el largo de los códigos y nos da todas las listas posibles con ese largo
allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = allColors
allCodes n = appendWithAll $ allCodes (n-1)

-- recuerda: el número de combinaciones posibles es 6^largo, puesto que hay 6 colores en la lista colors

-- Exercise 7 -----------------------------------------

initial :: [Code]
initial = allCodes 4

solve' :: Code -> [Code] -> [Move]
solve' secret [] = []
solve' secret [x] = [getMove secret x]
solve' secret toda@(x:xs) = mov : solve' secret new_list
  where
    new_list = filterCodes mov xs
    mov = getMove secret x

solve :: Code -> [Move]
solve secret = solve' secret initial

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined