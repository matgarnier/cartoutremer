
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
polygone sont supportées ; quel que soit le système de projection du
sfdataframe en entrée, le sfdataframe généré par la fonction
`transfo_om` est projeté en RGF93 / Lambert-93 (EPSG:2154).

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
  - Wallis-et-Futuna (986)
  - Polynésie Française (987)
  - Nouvelle-Calédonie (988)

# Installation :

``` r
remotes::install_github("ARCEP-dev/cartoutremer")
```

# Exemple :

``` r
library(cartoutremer)

# import des contours des départements de France métropolitaine et des DROM en projection conventionnelle 
# et conversion en projection WGS 1984
# puis agrégation de l'ensemble des départements

DEP_FRMETDROM <- DEP_FRMET %>% st_transform(4326) %>%
  rbind.data.frame(DEP_971 %>% st_transform(4326)) %>%
  rbind.data.frame(DEP_972 %>% st_transform(4326)) %>%
  rbind.data.frame(DEP_973 %>% st_transform(4326)) %>%
  rbind.data.frame(DEP_974 %>% st_transform(4326)) %>%
  # rbind.data.frame(DEP_975) %>%
  rbind.data.frame(DEP_976 %>% st_transform(4326)) %>%
  rbind.data.frame(DEP_977 %>% st_transform(4326)) %>%
  rbind.data.frame(DEP_978 %>% st_transform(4326)) %>%
  # rbind.data.frame(DEP_986) %>%
  # rbind.data.frame(DEP_987) %>%
  # rbind.data.frame(DEP_988) %>%
  identity()

# transformation des DROM pour les afficher à proximité de la France métropolitaine
DEP_FRMETDROM.proches <-
  transfo_om(shape_origine = DEP_FRMETDROM,
             var_departement = "INSEE_DEP",
             type_transfo = "v1")

# cartographie avec ggplot
library(ggplot2)
ggplot() +
  geom_sf(data = DEP_FRMETDROM.proches)
```

<img src="man/figures/README-carto_frmetdrom-1.png" width="100%" />

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

# cartographie 
ggplot() +
  geom_sf(data = DEP_FRMETDROM.proches,
          aes(fill = insee_dep),
          show.legend = FALSE,
          lwd  = 0) +
  geom_sf(data = # agrégation des COM visuellement rapprochés
                DEP_975.proche %>%
                bind_rows(DEP_977_978.proche),
          fill = "red",
          color = NA) +
  coord_sf( datum = NA)
```

# Ajout des cartons

``` r
ggplot() +
  geom_sf(data = DEP_FRMETDROM.proches %>%
                # agrégation des COM visuellement rapprochés
                bind_rows(DEP_975.proche) %>%
                bind_rows(DEP_977_978.proche)) +
  # délimitations des zones
  geom_rect(data = param_cadres_om %>%
              filter(DEP %in% c("971","972","973","974", "975","976", "977", "978")) %>%
              filter(type_rapp %in% "v1"),
              aes(xmin = xmin, xmax = xmax, 
                  ymin = ymin, ymax = ymax, 
                  group = DEP, color = DEP),
              fill = NA,
              stroke = 1) +
  # affichage des étiquettes
    geom_text(data = param_cadres_om %>%
                filter(DEP %in% c("971","972","973","974", "975","976", "977", "978")) %>%
                filter(type_rapp %in% "v1"),
               aes(x = xmax-20000, 
                   y = ymax-15000, 
                   color = DEP,
                   label = DEP),
               fill = NA,
               size = 2.3,
               fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
```

# Ressources annexes :

- Contours des communes de France métropolitaine et DROM en projection
  WGS1984 mis à disposition par l’Arcep sur
  [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/contours-communes-france-administrative-format-admin-express-avec-arrondissements/)

- Contours des communes des COM (Saint-Pierre-et-Miquelon,
  Saint-Barthélémy, Saint-Martin) mis à disposition par l’Arcep sur
  [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/decoupage-administratif-des-com-st-martin-et-st-barthelemy-et-com-saint-pierre-et-miquelon-format-admin-express/)

- Contours des communes de
  [Wallis-et-Futuna](https://nauru-data.sprep.org/system/files/wallis-et-futuna_0.zip)
  , de [Polynésie
  Française](https://static.data.gouv.fr/resources/limites-geographiques-administratives/20220610-202135/shapefiles.zip)
  et de
  [Nouvelle-Calédonie](https://data.opendatasoft.com/explore/dataset/communes-nc-limites-terrestres-simplifiees@nouvelle-caledonie/download/?format=shp&timezone=Europe/Berlin&lang=fr).
