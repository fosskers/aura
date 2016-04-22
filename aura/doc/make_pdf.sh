#!/usr/bin/env sh

make latex
cd build/latex
xelatex *.tex
# run again to get toc
xelatex *.tex
