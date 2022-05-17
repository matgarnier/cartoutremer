#' @name transfo_om
#'
#' @title Transformer un territoire de France d'Outre-Mer pour le placer près de la France métropolitaine.
#'
#' @description Modifie le champ géométrie du sf dataframe en entrée.
#'
#' @param shape_origine Table géographique (classe sf dataframe) en entrée contenant les entités du territoire de France d'Outre-Mer. Les géométries de ces entités peuvent être de type point, ligne ou polygone.
#' @param var_departement Nom du champ de la table géographique en entrée contenant le code du département de France d'Outre-Mer.
#' @param type_transfo Version de la transformation à appliquer (une seule disponible pour l'instant : 'v1').
#'
#' @return Renvoie une table géographique (classe sf dataframe) contenant les entités du territoire de France d'Outre-Mer replacés.
#'
#' @importFrom dplyr filter rename
#' @importFrom rlang sym
#' @importFrom sf st_transform st_as_sf st_set_crs
#' @importFrom maptools elide
#'
#' @examples
#'
#' # Transformer la géométrie des communes d'un territoire d'Outre-Mer pour les afficher proche de la France métropolitaine
#'\dontrun{
#' COMM_DROM.proches <-
#' transfo_om(shape_origine = COMM_DROM,
#'            var_departement = "INSEE_DEP",
#'            type_transfo = "v1")
#'}
#'
#' @details
#'
#' Les codes de départements renseignés dans le champ 'var_departement' doivent être compris dans la liste suivante :
#' \itemize{
#' \item{'971' (Guadeloupe)}
#' \item{'972' (Martinique)}
#' \item{'973' (Guyane)}
#' \item{'974' (La Réunion)}
#' \item{'975' (St-Pierre-et-Miquelon)}
#' \item{'976' (Mayotte)}
#' \item{'977' (Saint-Barthélémy)}
#' \item{'978' (Saint-Martin)}}
#'
#' @export


transfo_om <- function(shape_origine, var_departement, type_transfo = "v1"){

  # controle de la classe du dataframe en entrée
  if (missing(shape_origine)) { stop("L'argument 'shape_origine' définissant l'objet en entrée doit être renseigné") }
  if (is(shape_origine,"data.frame")==F) { stop("L'objet en entrée doit être de classe 'data.frame'") }
  if (is(shape_origine,"sf")==F) { stop("L'objet en entrée doit être de classe 'sf'") }

  # controle du dataframe en entrée non vide
  if (nrow(shape_origine) == 0) { stop("L'objet en entrée ne doit pas être vide") }

  # controle de la présence du champ département
  if (missing(var_departement)) { stop("Le champ 'var_departement' de l'objet en entrée doit être renseigné") }
  # if (shape_origine %>% select(!!sym(var_departement)) %in% c("971","972","973","974","975", "976", "977", "978")) { stop("Les codes de départements renseignés doivent être compris dans la liste suivante : {971,972,973,974,975,976,977,978} ") }

  # controle de la présence du type de transformation
  if (missing(type_transfo)) { stop("L'argument 'type_transfo' doit être renseigné") }

  # conversion du type de géométrie si nécessaire
  # MULTISURFACE -> POLYGON
  if (any(st_geometry_type(shape_origine) %in% "MULTISURFACE")){
    shape_origine <- st_cast(shape_origine, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")
    warning("Les entités de l'objet en entrée ont été converties en type POLYGONE")
  }

  # nom de la colonne géometrie
  nom_col_geom <- attr(shape_origine, "sf_column")

  # reprojection en epsg 3857
  shape_origine <-
    shape_origine %>%
    st_transform(3857)

  # parametres de la transformation

  param_DROM_rapp <- param_transfo_om %>%
    filter(type_rapp %in% type_transfo)

  # transformation de la géometrie

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '971') %>% nrow() >0) {
    shape_971 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "971") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '972') %>% nrow() >0) {
    shape_972 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "972") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '973') %>% nrow() >0) {
    shape_973 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "973") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '974') %>% nrow() >0) {
    shape_974 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "974") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '975') %>% nrow() >0) {
    shape_975 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "975") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '976') %>% nrow() >0) {
    shape_976 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "976") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '977') %>% nrow() >0) {
    shape_977 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "977") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '978') %>% nrow() >0) {
    shape_978 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "978") %>%
      as(., 'Spatial') %>%
      elide(rot=param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(rotation),
            scale=param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(echelle)) %>%
      elide(shift=c(param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(shift_x),
                    param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(shift_y))) %>%
      st_as_sf(.) %>%
      st_set_crs(2154)
  }

  # conservation de la géometrie pour la france métropolitaine

  if (shape_origine %>% filter(!substr(as.character(!!sym(var_departement)),1,2) %in% '97') %>% nrow() >0) {
    shape_FRMET <-
      shape_origine %>%
      filter(!substr(!!sym(var_departement),1,2) %in% "97") %>%
      # st_set_crs(2154) %>%
      st_transform(2154) %>%
      identity()
    # renommage du champ de geometries
    st_geometry(shape_FRMET) <- "geometry"
  }

  shape_origine.rap <- rbind(if(exists("shape_971")) shape_971,
                             if(exists("shape_972")) shape_972,
                             if(exists("shape_973")) shape_973,
                             if(exists("shape_974")) shape_974,
                             if(exists("shape_975")) shape_975,
                             if(exists("shape_976")) shape_976,
                             if(exists("shape_977")) shape_977,
                             if(exists("shape_978")) shape_978,
                             if(exists("shape_FRMET")) shape_FRMET) %>%
    identity()

    # renommage du champ de geometries
  st_geometry(shape_origine.rap) <- nom_col_geom

  shape_origine.rap
}


