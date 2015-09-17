{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor()
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random()

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = do
  a <- m
  return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vec = do
  izquierda  <- vec V.!? i
  derecha <- vec V.!? j
  primerCambio <- return $ vec V.// [(i, derecha)]
  return $ primerCambio V.// [(j, izquierda)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM mf xs = sequence $ fmap mf xs

-- me devuelve un valor Maybe con los elementos de las posiciones dadas por la lista parámetro
-- V.!? vect :: (Int -> Maybe a) ó (a -> m b)
-- xs :: [Int] ó t a
-- m (t b) :: Maybe [a]
getElts :: [Int] -> Vector a -> Maybe [a]
getElts xs vec = mapM (vec V.!?) xs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec = case V.length vec of
                  0 -> return Nothing
                  x -> do randomIndex <- getRandomR (0, x - 1)
                          return $ vec V.!? randomIndex

-- Exercise 4 -----------------------------------------

-- getRandom :: Random a => Rnd a
-- getRandomR :: Random a => (a, a) -> Rnd a

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = do
  listaValoresMonadicos <- replicateM n getRandom
  return $ V.fromList listaValoresMonadicos

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (low, high) = do
  listaValoresMonadicos <- replicateM n $ getRandomR (low, high)
  return $ V.fromList listaValoresMonadicos

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = let iterations = V.length vec - 1
                  changeBy = [iterations, iterations - 1..1]
              in do
                   listOfChanges <- mapM (\i -> getRandomR (0, i)) changeBy
                   return $ foldl (\vector (i, j) -> case swapV i j vector of
                                                       Nothing -> V.empty
                                                       Just v -> v) vec $ zip listOfChanges changeBy

-- Función mía: Imprimir vector en mónada random
-- imprimirVectorEnMonada $ shuffle $ V.fromList([1..10])
imprimirVectorEnMonada :: (Show a) => Rnd (Vector a) -> IO ()
imprimirVectorEnMonada vecRand = let vector = evalRand vecRand (mkStdGen 0)
                                 in do
                                   putStrLn $ show vector

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec pos = let antesDelPivote = V.slice 0 (pos) vec
                          despuesDelPivote = V.slice (pos + 1) (V.length vec - pos - 1) vec
                          vectorSinPivote = antesDelPivote V.++ despuesDelPivote
                          valorEnPosPivote = vec V.! pos
                      in (V.filter (\x -> (compare x valorEnPosPivote) == LT) vectorSinPivote,
                          valorEnPosPivote, V.filter (\x -> (compare x valorEnPosPivote) /= LT) vectorSinPivote)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

-- importante!! los vectores también pueden usar list comprehensions!!
-- imprimirVectorEnMonada $ liftM qsort $ shuffle $ V.fromList([1..10])
qsort :: Ord a => Vector a -> Vector a
qsort vec = if V.null vec
              then V.empty
              else let pivote = V.head vec
                       cola = V.tail vec
                   in qsort [y | y <- cola, y < pivote]
                      <> (pivote `cons` qsort [y |y <- cola, y >= pivote])

-- Exercise 8 -----------------------------------------

-- shuffle :: Vector a -> Rnd (Vector a)
-- imprimirVectorEnMonada :: (Show a) => Rnd (Vector a) -> IO ()
-- La siguiente línea es incorrecta:
-- imprimirVectorEnMonada $ liftM qsortR $ shuffle $ V.fromList([1..10])
-- La siguiente línea es correcta, si averiguas por qué, descubrirás el poder del bind
-- imprimirVectorEnMonada $ (shuffle $ V.fromList([1..10])) >>= qsortR
qsortR :: (Show a, Ord a) => Vector a -> Rnd (Vector a)
qsortR vec = if V.null vec
               then return $ V.empty
               else let pivoteMaximoPosible = V.length vec - 1
                    in do
                      pivote <- getRandomR (0, pivoteMaximoPosible)
                      (menores, pivote, mayores) <- return $ partitionAt vec pivote
                      menoresOrdenados <- qsortR menores
                      mayoresOrdenados <- qsortR mayores
                      return $ menoresOrdenados <> (pivote `cons` mayoresOrdenados)

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100