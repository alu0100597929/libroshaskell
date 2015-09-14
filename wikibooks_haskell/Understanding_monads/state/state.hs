import Control.Monad
import System.Random
import Control.Monad.Trans.State

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n (randomRIO (1,6))

clumsyRollDice :: (Int, Int)
clumsyRollDice = (m, n)
        where
        (m, g) = randomR (1,6) (mkStdGen 0)
        (n, _) = randomR (1,6) g

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = let (m, g') = randomR (1, 6) g
                 (n, g'') = randomR (1, 6) g'
             in ((m, n), g'')

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where

return :: a -> State s a
return x = state ( \ st -> (x, st) )

(>>=) :: State s a -> (a -> State s b) -> State s b
pr >>= k = state $ \ st ->
   let (x, st') = runState pr st -- Running the first processor on st.
   in runState (k x) st'       -- Running the second processor on st'.