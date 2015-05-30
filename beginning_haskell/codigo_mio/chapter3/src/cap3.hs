{-# LANGUAGE ViewPatterns #-}

-- por mucho smart constructor que usemos, al final tenemos que exportar el
-- constructor que no es smart, puta bida

import Chapter3.Ranges

{-- Esta función no expone constructores-}
prettyRange :: Range -> String
prettyRange rng = case rng of
                    (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"

