module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

-- cambiar una función en un punto
extend :: State -> String -> Int -> State
extend state s' n' = (\str -> if str == s'
                                then n'
                                else state str)

empty :: State
empty = (\_ -> 0)

-- Exercise 2 -----------------------------------------

-- recuerda que 0 es False y 1 es True
evalE :: State -> Expression -> Int
evalE st exp = case exp of
                 (Var nombreVar)      -> st nombreVar
                 (Val x)              -> x
                 (Op expr1 bop expr2) -> execOP bop (evalE st expr1) (evalE st expr2)

execOP :: Bop -> Int -> Int -> Int
execOP (Plus) n m   = n + m
execOP (Minus) n m  = n - m
execOP (Times) n m  = n * m
execOP (Divide) n m = n `div` m
execOP (Gt) n m     = if n >  m then 1 else 0
execOP (Ge) n m     = if n >= m then 1 else 0     
execOP (Lt) n m     = if n <  m then 1 else 0
execOP (Le) n m     = if n <= m then 1 else 0
execOP (Eql) n m    = if n == m then 1 else 0

-- evalE empty (Op (Op (Val 3) Plus (Val 5)) Times (Val 4))
-- 32

-- evalE (extend empty "A" 5) (Op (Op (Var "A") Plus (Val 5)) Times (Val 4))
-- 40

extractVar :: Expression -> String
extractVar (Var str) = str

extractVal :: Expression -> Int
extractVal (Val int) = int

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign str expr) = DAssign str expr
desugar (Incr varName) = DAssign varName (Op (Var varName) Plus (Val 1))
desugar (If expr statement1 statement2) = DIf expr (desugar statement1) (desugar statement2)
desugar (While expr statement) = DWhile expr (desugar statement)
--desugar (For Statement Expression Statement Statement) = DWhile
desugar (Sequence statement1 statement2) = DSequence (desugar statement1) (desugar statement2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple current (DAssign str expr) = extend current str (evalE current expr) 

-- let s = evalSimple empty (DAssign "A" (Op (Val 10) Plus (Val 1))) in s "A"
-- 11

run :: State -> Statement -> State
run = undefined

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
