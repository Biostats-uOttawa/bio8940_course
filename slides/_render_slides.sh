#!/bin/bash

quarto render $1.qmd --to html --output $1_notes.html
quarto render $1.qmd --to revealjs --output $1_slides.html
Rscript _purl.r $1 $2