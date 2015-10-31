-- Hay veces en las que se requiere importar clases de tipos "raras"
import Control.Monad.Plus
import Control.Monad.Trans
import Control.Applicative.Alternative

import Data.Char (isAlpha, isNumber, isPunctuation)

getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

askPassphrase :: IO ()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   if isJust maybe_value
                     then do putStrLn "Storing in database..." -- do stuff
                     else putStrLn "Passphrase invalid."

-- Transformador monádico para Maybe. Es una mónada, que contiene otra mónada
-- runMaybeT :: MaybeT m a -> m (Maybe a)
-- MaybeT :: m (Maybe a) -> MaybeT m a
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  
  -- fmap :: (a -> b) -> m a -> m b
  -- MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  -- fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f transformer = MaybeT $ fmap (fmap f) $ runMaybeT transformer

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just

  gs <*> xs = MaybeT $ do maybe_function <- runMaybeT gs
                          maybe_value <- runMaybeT xs
                          case maybe_function of
                            Nothing -> return Nothing
                            Just f -> case maybe_value of
                              Nothing -> return Nothing
                              Just value -> return $ Just $ f value

instance Monad m => Monad (MaybeT m) where
    return = pure

    -- The signature of (>>=), specialized to MaybeT m
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> return Nothing
                               Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
  empty = mzero

  (<|>) = mplus

instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                            case maybe_value of
                                 Nothing    -> runMaybeT y
                                 Just _     -> return maybe_value

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)