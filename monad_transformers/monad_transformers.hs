-- monad_transformers.hs

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

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }