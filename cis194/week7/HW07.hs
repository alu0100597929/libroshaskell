import qualified Data.Vector as V
import Control.Monad

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = do
  a <- m
  return $ f a

swapV :: Int -> Int -> V.Vector a -> Maybe (V.Vector a)
swapV i j vec = do
  izquierda  <- vec V.!? i
  derecha <- vec V.!? j
  primerCambio <- return $ vec V.// [(i, derecha)]
  return $ primerCambio V.// [(j, izquierda)]

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM mf xs = sequence $ fmap mf xs