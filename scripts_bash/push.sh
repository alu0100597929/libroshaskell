#!/bin/bash

if [ "$1" != "" ]; then
    cd TFG
    ./crear_pdf.sh
    cd ..
    git init
    git add *
    git commit -am "$1"
    git push origin master
else
    echo "El script debe recibir un parámetro con la string que describe al commit, entre \"\""
fi