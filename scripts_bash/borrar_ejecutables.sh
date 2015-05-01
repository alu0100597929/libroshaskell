#!/bin/bash

#se carga los ficheros .o y .hi
find -name "*.o" -type f -delete
find -name "*.hi" -type f -delete

#se carga todo fichero que no contenga "." en su nombre
find -not -name "*.*" -type f -delete