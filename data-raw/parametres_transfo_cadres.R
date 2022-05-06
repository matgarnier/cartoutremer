## code to prepare `parametres_transfo_cadres` dataset goes here
library(tibble)
param_cadres_om <-  tribble(
  ~ DEP, ~ xmin,  ~ xmax,  ~ ymin, ~ ymax, ~type_rapp,
  "971",  15000, 180000, 6490000,6620000, "v1",
  "972",  180000, 305000, 6490000,6620000, "v1",
  "973",  15000, 180000,6260000,6490000, "v1",
  "974",  180000, 305000, 6350000,6490000,"v1",
  "976",  180000, 305000, 6260000,6350000,"v1")

# export
usethis::use_data(param_cadres_om, overwrite = TRUE)
