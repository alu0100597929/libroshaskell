-- module Chapter3.Ranges (Range(), range) where
module Chapter3.Ranges (Range(Range), RangeObs(R), r) where --Range(Range), 

data Range = Range Integer Integer deriving Show

-- esta función es un smart constructor, es decir, es un constructor que comprueba lo que se va a crear
-- si es correcto, lo construye, si no, da error
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

-- el problema de esto es que cualquier cosa fuera del código privado no
-- compilará, dado que el constructor está oculto, la solución es crear un
-- nuevo tipo de dato que codifica los valores observados de ese tipo y luego
-- usa vistas cuando hace reconocimiento de patrones

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b