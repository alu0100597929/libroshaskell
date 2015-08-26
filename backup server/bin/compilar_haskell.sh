#!/bin/bash

ghc --make interprete.hs

## Uso del intérprete

# meter expresiones válidas como las que se encuentran comentadas en el código:

# Lisp>>> (case (+ 5 5) ((4 9 1) 'd64)\n((1 2) 'pepito)\n((10) 'jorgito))
# jorgito
# Lisp>>> (cond ((> 3 2) 'greater)\n((< 3 2) 'less))
# greater
# Lisp>>> (cond ((> 3 3) 'greater)\n((< 3 3) 'less)\n(else 'equal))
# equal
# Lisp>>> (cond ((> 3 3) 'greater)\n((< 3 3) 'less)\n(else 'equal))
# equal

# para salir del intérprete escribir "quit"

# en el intérprete no se puede borrar de momento nada de lo que escribes, si la armas
# le das a intro y lo intentas de nuevo.
