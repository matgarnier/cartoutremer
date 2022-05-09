
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cartoutremer

<!-- badges: start -->

<!-- badges: end -->

Le package R `CartOutremer` permet de faciliter la cartographie des
territoires français d’Outre-Mer (DROM et COM) dans les outils R et
QGis. Ces territoires sont placés à proximité de la France
métropolitaine et leur échelle est altérée afin de faciliter la
lisibilité des cartes produites.

Les transformations d’entités géographiques de type point, ligne et
polygone sont supportées.

Les territoires de France d’Outre-Mer inclus sont les suivants :

  - l’ensemble des DROM (Départements et Régions d’outre-mer)
      - Guadeloupe (971)
      - Martinique (972)
      - Guyane (973)
      - La Réunion (974)
      - Mayotte (976)
  - les COM (Collectivités d’outre-mer) suivantes :
      - Saint-Pierre-et-Miquelon (975)
      - Saint-Barthélémy (977)
      - Saint-Martin (978)

# Installation :

``` r
remotes::install_github("ARCEP-dev/cartoutremer")
```

# Exemple :

``` r
library(cartoutremer)

# import des contours des départements de France métropolitaine et des DROM en projection WGS1984 via l'API IGN 
library(httr)
api_ignadmin <- "https://wxs.ign.fr/administratif/geoportail/wfs"
url <- parse_url(api_ignadmin)
url$query <- list(service = "wfs",
                  request = "GetFeature",
                  srsName = "EPSG:4326",
                  typename = "ADMINEXPRESS-COG.LATEST:departement")

DEP_FRMETDROM <- build_url(url) %>% read_sf() %>% select(-gml_id, -insee_reg)

# sélection des départements de France métropolitaine et conversion en projection conforme (RGF 93)
DEP_FRMET <-
  DEP_FRMETDROM %>%
  filter(!substr(insee_dep,1,2) %in% "97") %>%
  st_transform(2154)

# transformation des DROM pour les afficher à proximité de la France métropolitaine
DEP_FRDROM.proches <-
  transfo_om(shape_origine = DEP_FRMETDROM %>%
                             # uniquement les DROM
                             filter(substr(insee_dep,1,2) %in% "97"),
             var_departement = "insee_dep",
             type_transfo = "v1")

colnames(DEP_FRMET)
#> [1] "id"        "nom_m"     "nom"       "insee_dep" "the_geom"
colnames(DEP_FRDROM.proches)
#> [1] "id"        "nom_m"     "nom"       "insee_dep" "geometry"

# cartographie avec ggplot 
# library(ggplot2)
# ggplot() +
#   geom_sf(data = DEP_FRMET %>%
#                 # agrégation des DROM visuellement rapprochés
#                 rbind.data.frame(DEP_FRDROM.proches))
```

``` r
# ajout des COM 975/977/978

DEP_977_978 <- st_read("https://static.data.gouv.fr/resources/decoupage-administratif-des-com-st-martin-et-st-barthelemy-et-com-saint-pierre-et-miquelon-format-admin-express/20220506-142254/departement.geojson",quiet = TRUE) %>%
  # mise en cohérence des champs
select(id=ID, nom_m = NOM_DEP, nom = NOM_DEP_M, insee_dep = INSEE_DEP, the_geom = geometry)

DEP_975 <- st_read("https://static.data.gouv.fr/resources/decoupage-administratif-des-com-st-martin-et-st-barthelemy-et-com-saint-pierre-et-miquelon-format-admin-express/20220506-142220/departement.geojson",quiet = TRUE) %>%
  # mise en cohérence des champs
select(id=ID, nom_m = NOM_DEP, nom = NOM_DEP_M, insee_dep = INSEE_DEP, the_geom = geometry)
  
# transformation des DROM pour les afficher à proximité de la France métropolitaine
DEP_977_978.proche <-
  transfo_om(shape_origine = DEP_977_978,
             var_departement = "insee_dep",
             type_transfo = "v1")
# colnames(DEP_977_978.proche)
DEP_975.proche <-
  transfo_om(shape_origine = DEP_975,
             var_departement = "insee_dep",
             type_transfo = "v1")
# colnames(DEP_975.proche)
# colnames(DEP_FRMET)
# cartographie 
ggplot() +
  geom_sf(data = DEP_FRMET %>%
                # agrégation des DROM visuellement rapprochés
                rbind.data.frame(DEP_FRDROM.proches) %>%
                # agrégation des COM visuellement rapprochés
                rbind.data.frame(DEP_975.proche) %>%
                rbind.data.frame(DEP_977_978.proche),
          aes(fill = insee_dep),
          show.legend = FALSE,
          lwd  = 0) +
  coord_sf(crs = 2154, datum = NA)
```

# Ajout des cartons

``` r
ggplot() +
  geom_sf(data = DEP_FRMET %>%
                # agrégation des DROM visuellement rapprochés
                rbind.data.frame(DEP_FRDROM.proches)) +
  geom_rect(data = param_cadres_om,
              aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax, 
                  group = DEP),
              fill = NA,
              color = "grey60",
              alpha = 1)
```

# Ressources annexes :

  - Contours des communes de France métropolitaine et DROM en projection
    WGS1984 mis à disposition par l’Arcep sur
    [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/contours-communes-france-administrative-format-admin-express-avec-arrondissements/)

  - Contours des communes des COM (Saint-Pierre-et-Miquelon,
    Saint-Barthélémy, Saint-Martin) mis à disposition par l’Arcep sur
    [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/decoupage-administratif-des-com-st-martin-et-st-barthelemy-et-com-saint-pierre-et-miquelon-format-admin-express/)
