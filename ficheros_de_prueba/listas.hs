import Data.List
import Data.Maybe

posSubLista :: (Eq a) => [a] -> [a] -> Maybe Int
posSubLista sub s
  | length sub > length s      = Nothing
  | take (length sub) s == sub = Just 0
  | otherwise                  = fmap (+1) $ posSubLista sub $ drop 1 s