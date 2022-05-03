
library(devtools)
library(usethis)
library(testthat)
library(pkgdown)

options(stringsAsFactors = FALSE)

# licence
use_mit_license("arcep")

usethis::use_readme_rmd( open = TRUE )

# remplir DESCRIPTION
edit_file("DESCRIPTION")

# données
usethis::use_data_raw( name = "parametres_transfo_geom", open = TRUE )


### fonctions
use_r("transfo_om")

# dependences
usethis::use_package("dplyr")
usethis::use_package("sf")
usethis::use_package("maptools")
usethis::use_package("rlang")

# check
devtools::check()

# git
use_git()
