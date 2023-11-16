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
#' @importFrom sf st_transform st_as_sf st_set_crs st_bbox st_geometry st_crs st_combine st_collection_extract st_intersection st_cast st_union
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
#' \item{'986' (Wallis-et-Futuna)}}
#' \item{'987' (Polynésie Française)}}
#' \item{'988' (Nouvelle-Calédonie)}}
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

  # # conversion du type de géométrie si nécessaire
  # # MULTISURFACE -> POLYGON
  # if (any(st_geometry_type(shape_origine) %in% "MULTISURFACE")){
  #   shape_origine <- st_cast(shape_origine, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")
  #   warning("Les entités de l'objet en entrée ont été converties en type POLYGONE")
  # }

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
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(shift_y)),
                       mask = get("DEP_971") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '971') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '972') %>% nrow() >0) {
    shape_972 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "972") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(shift_y)),
                       mask = get("DEP_972") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '972') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '973') %>% nrow() >0) {
    shape_973 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "973") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(shift_y)),
                       mask = get("DEP_973") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '973') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '974') %>% nrow() >0) {
    shape_974 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "974") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(shift_y)),
                       mask = get("DEP_974") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '974') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '975') %>% nrow() >0) {
    shape_975 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "975") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(shift_y)),
                       mask = get("DEP_975") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '975') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '976') %>% nrow() >0) {
    shape_976 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "976") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(shift_y)),
                       mask = get("DEP_976") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '976') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '977') %>% nrow() >0) {
    shape_977 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "977") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(shift_y)),
                       mask = get("DEP_977") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '977') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '978') %>% nrow() >0) {
    shape_978 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "978") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(shift_y)),
                       mask = get("DEP_978") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '978') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '986') %>% nrow() >0) {
    shape_986 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "986") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '986') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '986') %>% pull(shift_y)),
                       mask = get("DEP_986") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '986') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '986') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '987') %>% nrow() >0) {
    shape_987 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "987") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '987') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '987') %>% pull(shift_y)),
                       mask = get("DEP_987") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '987') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '987') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  if (shape_origine %>% filter(as.character(!!sym(var_departement)) %in% '988') %>% nrow() >0) {
    shape_988 <-
      shape_origine %>%
      filter(!!sym(var_departement) %in% "988") %>%
      f_moveresize_tot(x = . ,
                       xy = c(param_DROM_rapp %>% filter(DEP %in% '988') %>% pull(shift_x),
                              param_DROM_rapp %>% filter(DEP %in% '988') %>% pull(shift_y)),
                       mask = get("DEP_988") %>% st_transform(3857),
                       r = param_DROM_rapp %>% filter(DEP %in% '988') %>% pull(rotation),
                       k = param_DROM_rapp %>% filter(DEP %in% '988') %>% pull(echelle)) %>%
      st_set_crs(3857) %>%
      st_transform(2154) %>%
      identity()
  }

  # conservation de la géometrie pour la france métropolitaine

  if (shape_origine %>% filter(!substr(as.character(!!sym(var_departement)),1,2) %in% '97') %>% nrow() >0) {
    shape_FRMET <-
      shape_origine %>%
      filter(!substr(!!sym(var_departement),1,2) %in% c("97","98")) %>%
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
                             if(exists("shape_986")) shape_986,
                             if(exists("shape_987")) shape_987,
                             if(exists("shape_988")) shape_988,
                             if(exists("shape_FRMET")) shape_FRMET) %>%
    identity()

  # renommage du champ de geometries
  st_geometry(shape_origine.rap) <- nom_col_geom

  shape_origine.rap
}


# fonction basée sur sf uniquement
# à partir du package "riatelab/mapinsetr"
# avec ajout du paramètre rotation


f_moveresize_tot <- function (x, mask = NULL, xy, prj,r, k = 1)
{
  if (missing(prj)) {
    prj <- st_crs(x)
  }
  if (missing(mask)) {
    mask <- st_union(x)
  }
  stopifnot(!is.na(st_crs(mask)), !is.na(st_crs(x)))
  mask <- st_union(mask)
  if (class(st_geometry(x))[1] == "sfc_POINT") {
    x <- f_moveresize_pt(x = x, mask = mask, xy = xy,
                         prj = prj, k = k)
    return(x)
  }
  namesorder <- names(x)
  cp <- class(st_geometry(x))[1] == "sfc_MULTIPOLYGON"
  x <- suppressWarnings(st_intersection(x, st_geometry(mask)))
  xm <- x[1, ]
  st_geometry(xm) <- st_geometry(mask)
  x <- rbind(xm, x)
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) * k + cntrd[[1]][]
  st_geometry(x) <- xg + xy - st_bbox(xg)[1:2]
  x <- x[-1, ]

  if (cp) {
    x <- st_cast(x, "MULTIPOLYGON")
  }


  st_crs(x) <- prj
  x <- x[, namesorder]

  # rotate
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  st_geometry(x) = (st_geometry(x) - st_centroid(st_geometry(x))) * rot(pi/r) * .95 + st_centroid(st_geometry(x))

  return(x)
}

f_moveresize_pt <- function(x, mask, xy, prj, k){
  # names order mngmt
  namesorder <- names(x)

  # intersect mask and x
  x <- suppressWarnings(
    st_collection_extract(
      st_intersection(x, st_geometry(mask)),
      type = c("POINT")
    )
  )

  # add mask to x
  xm <- x[1, ]
  st_geometry(xm) <- st_geometry(mask)
  x <- rbind(xm,x)

  # resize & move
  cntrd <- st_centroid(st_combine(x))
  xg <- (st_geometry(x) - cntrd) * k + cntrd[[1]][]
  st_geometry(x) <- xg + xy - st_bbox(xg)[1:2]

  # rotate
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  st_geometry(x) = (st_geometry(x) - st_centroid(st_geometry(x[1,]))) * rot(pi/r) * .95 + st_centroid(st_geometry(x[1,]))

  # get rid of mask
  x <- x[-1,]
  st_crs(x) <- prj

  # names order mngmt
  x <- x[, namesorder]

  return(x)
}
