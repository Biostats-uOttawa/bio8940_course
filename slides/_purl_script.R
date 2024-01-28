files <- list.files(pattern = "Rmd")

lec_purl <- c(4)

for (file in files[lec_purl]) {
  knitr::purl(
    file,
    output = paste0("../data/R_code/", gsub(".Rmd", ".R", file))
  )
}
