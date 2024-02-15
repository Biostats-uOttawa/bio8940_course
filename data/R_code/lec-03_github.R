## ----echo = FALSE-------------------------------------------------------------
library(gitcreds)
library(usethis)


## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::git_sitrep()


## ----eval=FALSE, echo = TRUE--------------------------------------------------
## usethis::use_git_config(
##    user.name = "JulienGAMartin",
##    user.email = "julien.martin@uottawa.ca")


## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::create_github_token()


## ----eval = FALSE, echo = TRUE------------------------------------------------
## gitcreds::gitcreds_set()


## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::use_git()


## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::use_github()
## usethis::git_vaccinate()


## git init
## git add .
## git commit -a -m "first commit"

## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::use_github()
## usethis::git_vaccinate()


## ----eval = FALSE, echo = TRUE------------------------------------------------
## usethis::create_from_github(
##   "https://github.com/your_github/your_repo.git",
##   destdir=".")


## ----eval=FALSE---------------------------------------------------------------
## usethis::use_github(protocol = "https")

