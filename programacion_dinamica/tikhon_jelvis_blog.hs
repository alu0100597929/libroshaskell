import Data.Array
import Data.List
import Data.Ord

type Distance = Int

data Action = None | Add | Remove | Modify deriving Show

script :: Eq a => (Action -> Distance) -> [a] -> [a] -> [Action]
script cost a b = reverse . snd $ d m n
  where (m, n) = (length a, length b)
        a'     = listArray (1, m) a
        b'     = listArray (1, n) b

        d 0 0 = (0, [])
        d i 0 = go (i - 1) 0 Remove
        d 0 j = go 0 (j - 1) Add
        d i j
          | a' ! i ==  b' ! j = go (i - 1) (j - 1) None
          | otherwise = minimum' [ go (i - 1) j       Remove
                                 , go i (j - 1)       Add
                                 , go (i - 1) (j - 1) Modify
                                 ]

        minimum' = minimumBy (comparing fst)
        go i j action = let (score, actions) = ds ! (i, j) in
          (score + cost action, action : actions)

        ds = listArray bounds [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

cost :: Action -> Distance
cost None = 0
cost _    = 1

main = do
          putStrLn "Cadena 1:"
          cad1 <- getLine
          putStrLn "Cadena 2:"
          cad2 <- getLine
          putStrLn $ show $ script cost cad1 cad2 