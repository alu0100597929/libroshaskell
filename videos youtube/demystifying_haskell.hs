{-vídeos interesantes sobre Haskell, con código y explicaciones-}

fibs = 1 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]

--pequeña traza:
--fibs = [1,1] : zip [1,1] [1] o sea (1,1)

{-variable en programación imperativa: trozo de memoria mutable con un nombre
  variable en Haskell, simplemente un nombre que usaremos para la sustitución
  el valor en Haskell es una forma de decir que es algo permanente.
  En haskell se definen las variables, no se asignan. Por ello, se hace sólo
  una vez, y eso no puede cambiar a lo largo de la ejecución.
  Una compresión de listas equivale a un "para todo x" en matemáticas
  El reconocimiento de patrones es un "binding"
  Haskell es muy fiel a las matemáticas reales, teóricas
-}

--fibs' = 1 : 1 : [ 2, 3, 5, 8 ]
fibs' = 1:1:[2,3,5,8]
--[1,1,2,3,5,8]
t = tail fibs'
--[1,2,3,5,8]
z = zip fibs' (tail fibs')
--[(1,1),(1,2),(2,3),(3,5),(5,8)]