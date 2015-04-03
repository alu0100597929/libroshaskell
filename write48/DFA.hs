-- cómo es un DFA? tiene una estado inicia, una tabla de estados
-- y un cjto. de estados finales

module DFA where
  type State = Int -- recuerda, los sinónimos de tipo siempre van con type
  type Start = State
  type StateTable = [((Char, State), State)]
  type Finals = [State]
  type DFA = (Start, StateTable, Finals)

  exampleFinals :: Finals
  exampleFinals = [1]

  exampleTable :: StateTable
  exampleTable = [(('0', 1), 2),
                  (('1', 1), 1),
                  (('0', 2), 1),
                  (('1', 2), 2)]

  transit :: (Char, State) -> State
  transit (c,s) = case lookup (c,s) exampleTable of
                    (Just s) -> s
                    _        -> error "language not supported"

  computar :: String -> Bool
  computar cad = False