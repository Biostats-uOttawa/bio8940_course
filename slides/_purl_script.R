files <- list.files(pattern = ".[Rq]md$")

lec_purl <- c(5)

for (file in files[lec_purl]) {
  knitr::purl(
    file,
    output = paste0("../data/R_code/", gsub(".[Rq]md$", ".R", file))
  )
  pagedown::chrome_print(gsub(".[Rq]md$", ".html", file))
}
