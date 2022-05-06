## code to prepare `parametres_transfo_geom` dataset goes here

library(tibble)

param_transfo_OM <- tribble(
  ~ DEP, ~ rotation,  ~ echelle,  ~ shift_x, ~ shift_y, ~type_rapp,
  "971",  0, 120000, 45000,6500000, "v1",
  "972",  0, 90000, 205000,6500000, "v1",
  "973",  0, 185000, 20000,6280000, "v1",
  "974",  0, 100000, 195000,6370000,"v1",
  "975",  0, 80000, 38000,6720000,"v1",
  "976",  0, 55000, 225000,6280000,"v1",
  "977",  0, 80000, 115000,6640000,"v1",
  "978",  0, 80000, 25000,6640000,"v1"
)

# export
usethis::use_data(param_transfo_OM, overwrite = TRUE)
