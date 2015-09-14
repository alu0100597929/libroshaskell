-- Se ilustra la capacidad de Maybe para "cortocircuitar" computaciones

import Control.Monad((<=<))
import Data.Maybe(fromMaybe)

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
  | x > 0     = Just (log x)
  | otherwise = Nothing

safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt x
  | x > 0     = Just (sqrt x)
  | otherwise = Nothing

safeLogSqrt = safeLog <=< safeSqrt

phonebook :: [(String, String)]
phonebook = [ ("Bob",   "01788 665242"),
              ("Fred",  "01624 556442"),
              ("Alice", "01889 985333"),
              ("Jane",  "01732 187565") ]

governmentDatabase :: [(String, String)]
governmentDatabase = [ ("01788 665242", "1"),
                       ("01624 556442", "2"),
                       ("01889 985333", "3"),
                       ("01732 187565", "4") ]

getRegistrationNumber :: String       -- their name
                      -> Maybe String -- their registration number
getRegistrationNumber name = 
  lookup name phonebook >>=
    (\number -> lookup number governmentDatabase)

antiguosOwners :: [(String, String)]
antiguosOwners = [("1", "Topo"),
                  ("2", "Topor"),
                  ("3", "Toportero"),
                  ("4", "Iker Topillas")]

getAntiguoOwner :: String       -- their name
                -> Maybe String -- their registration number
getAntiguoOwner name = 
  lookup name phonebook >>=
    (\number -> lookup number governmentDatabase) >>=
      (\secondNumber -> lookup secondNumber antiguosOwners)

getAntiguoOwner' :: String       -- their name
                 -> Maybe String -- their registration number
getAntiguoOwner' name = do
  number <- lookup name phonebook
  secondNumber <- lookup number governmentDatabase
  lookup secondNumber antiguosOwners

-- Mónada abierta

zeroAsDefault :: Maybe Int -> Int
zeroAsDefault mx = case mx of
    Nothing -> 0
    Just x -> x

zeroAsDefault' mx = fromMaybe 0 mx

displayResult :: Maybe Int -> String
displayResult mx = maybe "There was no result" (("The result was " ++) . show) mx