module DFA where

  -- se podría hacer con "tipos dinámicos"
  data DFA = DFA
    { intialState :: Int
    , isAccepting :: Int -> Bool
    , transition  :: Int -> Char -> Int
    }

  -- estado inicial
  i = 1

  -- criterio de aceptación
  a = (`elem` [1])

  -- tabla de transiciones, el lenguaje es aquel formado por cadenas a as y bes con
  -- un número par de as
  t 1 'A' = 2
  t 1 'B' = 1
  t 2 'A' = 1
  t 2 'B' = 2
  t _ _   = error "el símbolo no pertenece al lenguaje"

  dfa = DFA i a t

  testDFA :: DFA -> [Char] -> Bool
  testDFA (DFA i a t) = a . foldl t i