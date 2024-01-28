#!/usr/bin/env Rscript

renderthis::to_pdf(
  from = "lectures/00-intro_class/L1_intro_xar.Rmd"
)
renderthis::to_pdf(
  from = "lectures/01-linear_model_revision/LM_revision_slides.Rmd",
  complex_slides = TRUE # ,
  # partial_slides = TRUE
)
