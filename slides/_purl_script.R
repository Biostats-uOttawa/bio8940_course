purl_pdf <- function(files_list, purl = TRUE, pdf = TRUE) {
  for (file in files_list) {
    if (isTRUE(purl)) {
      knitr::purl(
        file,
        output = paste0("../data/R_code/", gsub(".[Rq]md$", ".R", file))
      )
    }
    if (isTRUE(pdf)) {
      pagedown::chrome_print(
        gsub(".[Rq]md$", ".html", file),
        timeout = 120
      )
    }
  }
}

files <- list.files(pattern = ".[Rq]md$")

purl_pdf(files[5],purl=FALSE)
