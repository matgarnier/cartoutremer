


transfo_om <- function(shape_origine, var_departement, type_transfo){

  # shape_origine <- DEP_GEO_971
  # type_rappDROM <- "v3"
  # var_departement <- "971"

  # vérification de la géometrie

  # st_crs(shape_origine)$input

  # nom de la colonne géometrie
  nom_col_geom <- attr(shape_origine, "sf_column")

  # reprojection en epsg 3857
  shape_origine <-
    shape_origine %>%
    st_transform(3857)

  # parametres de la transformation

  param_DROM_rapp <- param_transfo_OM %>%
    filter(type_rapp %in% type_transfo)

  # transformation de la géometrie

  if (shape_origine %>% filter(!!sym(var_departement) %in% '971') %>% nrow() >0) {
    shape_971 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "971") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      # rename(geom = geometry) %>%
      st_set_crs(3857)
  }

  if (shape_origine %>% filter(!!sym(var_departement) %in% '972') %>% nrow() >0) {
    shape_972 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "972") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(3857)
  }

  if (shape_origine %>% filter(!!sym(var_departement) %in% '973') %>% nrow() >0) {
    shape_973 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "973") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(3857)
  }

  if (shape_origine %>% filter(!!sym(var_departement) %in% '974') %>% nrow() >0) {
    shape_974 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "974") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(3857)
  }

  if (shape_origine %>% filter(!!sym(var_departement) %in% '976') %>% nrow() >0) {
    shape_976 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "976") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(3857)
  }

  shape_origine.rap <- rbind(if(exists("shape_971")) shape_971,
                             if(exists("shape_972")) shape_972,
                             if(exists("shape_973")) shape_973,
                             if(exists("shape_974")) shape_974,
                             if(exists("shape_976")) shape_976) %>%
    rename(!!sym(nom_col_geom) == geometry) %>%
    identity()



}


