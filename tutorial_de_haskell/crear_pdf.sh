#!/bin/bash

pandoc -V geometry:margin=1in -o Tutorial_de_Haskell.pdf --latex-engine=xelatex tutorial_haskell.md
evince Tutorial_de_Haskell.pdf 2>/dev/null >/dev/null &