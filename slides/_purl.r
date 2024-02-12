#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  file <- paste0(args[1], ".qmd")
} else {
  file <- paste0(args[1], args[2])
}

knitr::purl(
  file,
  output = paste0("../data/R_code/", gsub(".[Rq]md$", ".R", file))
)
