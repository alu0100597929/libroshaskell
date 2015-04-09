toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
        

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)

--recuerda, hay que doblar los números de posiciones impares (empezando
--desde el 0, empezando por la derecha)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther (x:[])     = [x]
doubleEveryOther (x:(y:zs)) = x : 2*y : doubleEveryOther zs

doblar :: Integer -> [Integer]
doblar n = reverse (doubleEveryOther (toDigitsRev n))

sumacifras :: Integer -> Integer
sumacifras n 
  | n <= 0 = 0
  | otherwise = sumacifras (n `div` 10) + (n `mod` 10)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sumacifras x | x<- xs]

validate :: Integer -> Bool
validate n = (sumDigits (doblar n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a c b = hanoi (n - 1) a b c
                ++ (a,c) : hanoi (n - 1) b c a

-- TODO
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a d b c = hanoi4 (n - 1) a b c d
                ++ (a,d) : hanoi4 (n - 1) b d b c