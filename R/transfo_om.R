#' @name transfo_om
#'
#' @title Transformer un territoire de France d'Outre-Mer pour le placer près de la France métropolitaine.
#'
#' @description Modifie le champ géometrie du sf dataframe en entrée.
#'
#' @param shape_origine Table géographique (format sf dataframe) en entrée contenant les entités du territoire de France d'Outre-Mer.
#' @param var_departement Nom du champ de la table géographique en entrée contenant le code du département de France d'Outre-Mer.
#' @param type_transfo Version de la transformation à appliquer.
#'
#' @return Renvoie une table géographique (format sf dataframe) contenant les entités du territoire de France d'Outre-Mer replacés.
#'
#' @importFrom dplyr filter rename
#' @importFrom rlang sym
#' @importFrom sf st_transform st_as_sf st_set_crs
#' @importFrom maptools elide
#'
#'
#' @export


transfo_om <- function(shape_origine, var_departement, type_transfo){

  # controle de la classe du dataframe en entrée
  if (!class(shape_origine) %in% "data.frame") { stop("L'objet en entrée doit être de classe 'data.frame'") }
  if (!class(shape_origine) %in% "sf") { stop("L'objet en entrée doit être de classe 'sf'") }

  # controle du dataframe en entrée non vide
  if (nrow(shape_origine) == 0) { stop("L'objet en entrée ne doit pas être vide") }

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
    rename(!!(nom_col_geom) := geometry) %>%
    identity()



}


