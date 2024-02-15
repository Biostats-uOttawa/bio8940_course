#!/bin/bash

quarto render $1.qmd --to html --output $1_notes.html
quarto render $1.qmd --to revealjs --output $1_slides.html
Rscript _purl.r $1 $2


echo purl code and rendered slides and notes for $1

if test -f $1.html
then
  rm $1.html
  echo removed $1.html
fi
