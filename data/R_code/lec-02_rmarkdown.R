## flowchart TB
##   a(qmd/Rmd)
##   yaml(YAML frontmatter)
##   md(Markdown content)
##   knitr(Code chunks)
##   a --- yaml & md & knitr

## flowchart TB
##   a(qmd/Rmd)
##   yaml(YAML frontmatter)
##   md(Markdown content)
##   knitr(Code chunks)
##   a --- yaml & md & knitr
##   style md fill:#f9f,stroke:#333,stroke-width:4px

## flowchart TB
##   a(qmd/Rmd)
##   yaml(YAML frontmatter)
##   md(Markdown content)
##   knitr(Code chunks)
##   a --- yaml & md & knitr
##   style knitr fill:#f9f,stroke:#333,stroke-width:4px

## -----------------------------------------------------------------------------
# cars is a built-in-to-R data set of cars and their stopping distances
cars %>%
  head(4) %>%
  knitr::kable(format = "html", caption = "A kable table")


## flowchart TB
##   a(qmd/Rmd)
##   yaml(YAML frontmatter)
##   md(Markdown content)
##   knitr(Code chunks)
##   a --- yaml & md & knitr
##   style yaml fill:#f9f,stroke:#333,stroke-width:4px

## ----out.width="75%", echo = FALSE--------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/crsh/citr/master/tools/images/addin_demo.gif")

