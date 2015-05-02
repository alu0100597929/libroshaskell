type Nombre = String
 
data Aniversario = Cumpleanios Nombre Fecha
                 | Boda Nombre Nombre Fecha
 
data Fecha = Fecha Int Int Int   -- Año, mes, día
 
pacoPerez :: Aniversario
pacoPerez = Cumpleanios "Paco Pérez" (Fecha 1968 7 3)
 
bodaPaco :: Aniversario
bodaPaco = Boda "Paco Pérez" "Juana López" (Fecha 1987 3 4)
 
type ListaAniversarios = [Aniversario]
 
anniversariesOfpacoPerez :: ListaAniversarios
anniversariesOfpacoPerez = [pacoPerez, bodaPaco]
 
mostrarFecha :: Fecha -> String
mostrarFecha (Fecha a m d) = show a ++ "-" ++ show m ++ "-" ++ show d 
 
mostrarAniversario :: Aniversario -> String
mostrarAniversario (Cumpleanios nombre fecha) =
   nombre ++ " born " ++ mostrarFecha fecha
mostrarAniversario (Boda nombre1 nombre2 fecha) =
   nombre1 ++ " married " ++ nombre2 ++ " on " ++ mostrarFecha fecha