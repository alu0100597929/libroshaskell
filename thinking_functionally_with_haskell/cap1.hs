import Data.Char
import Data.List

type Text = [Char]

mayorAMenor :: (Int, a) -> (Int, a) -> Ordering
mayorAMenor (a, _) (b, _) = compare b a

commonWords :: Int -> Text -> [(Int, String)]
commonWords n = take n . sortBy mayorAMenor . map (\xs -> (length xs, head xs)) . group . sort . words . map toLower

units, teens, tens :: [String]
units = ["zero","one","two","three","four","five","six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

convert1 n = units !! n

digits2 n = (n `div` 10, n `mod` 10)

convert2 :: Int -> String
convert2 = combine2 . digits2

combine2 :: (Int,Int) -> String
combine2 (t,u)
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | 2 <= t && u == 0 = tens !! (t - 2)
  | 2 <= t && u /= 0 = tens !! (t - 2) ++ "-" ++ units !! u

convert3 :: Int -> String
convert3 n
  | h==0 = convert2 t
  | n==0 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where (h,t) = (n `div` 100, n `mod` 100)

link :: Int -> String
link h = if h < 100 then " and " else " "

convert6 :: Int -> String
convert6 n
  | m == 0      = convert3 h
  | h == 0      = convert3 m ++ " thousand"
  | otherwise   = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where (m, h) = (n `div` 1000, n `mod` 1000)

-- EXERCISES