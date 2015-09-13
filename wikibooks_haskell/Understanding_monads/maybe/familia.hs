-- estas funciones reciben un tipo no monádico, y dan otro que sí es monádico
-- a -> m b
-- importante, en el comentario de la línea anterior la b está escrita por la razón del segundo párrafo
father :: Person -> Maybe Person
mother :: Person -> Maybe Person

--maternalGrandfather :: Person -> Maybe Person
--maternalGrandfather p =
--    case mother p of
--        Nothing -> Nothing
--        Just mom -> father mom

maternalGrandfather p = mother p >>= father

bothGrandfathers :: Person -> Maybe (Person, Person)
    bothGrandfathers p =
        case father p of
            Nothing -> Nothing
            Just dad ->
                case father dad of
                    Nothing -> Nothing
                    Just gf1 ->                          -- found first grandfather
                        case mother p of
                            Nothing -> Nothing
                            Just mom ->
                                case father mom of
                                    Nothing -> Nothing
                                    Just gf2 ->          -- found second grandfather
                                        Just (gf1, gf2)

bothGrandfathersDo :: Person -> Maybe (Person, Person)
bothGrandfathersDo p = do
  f1 <- father p
  ff1 <- father f1
  m1 <- mother p
  mm1 <- mother m1
  return (ff1, mm1)