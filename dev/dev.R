
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
usethis::use_data_raw( name = "parametres_transfo_cadres", open = TRUE )
usethis::use_data_raw( name = "dl_cartos_986_987_988", open = TRUE )


### fonctions
use_r("transfo_om")

# dependences
usethis::use_package("dplyr", type = "Depends")
usethis::use_package("sf", type = "Depends")
usethis::use_package("maptools", type = "Depends")
usethis::use_package("rlang", type = "Depends")


# tests unitaires
usethis::use_testthat()
usethis::use_test("transfo_om")

# check

devtools::check()
devtools::build()
# màj namespace
devtools::document()

# git
use_git()
