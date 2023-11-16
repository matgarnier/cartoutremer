## code to prepare `dl_cartos_986_987_988` dataset goes here
library(dplyr)
library(sf)
library(rmapshaper)

# Récupération des contours communaux des collectivités d'outre-mer suivantes :
# 986 : https://nauru-data.sprep.org/system/files/wallis-et-futuna_0.zip
# 987 : https://static.data.gouv.fr/resources/limites-geographiques-administratives/20220610-202135/shapefiles.zip
# 988 : https://data.opendatasoft.com/explore/dataset/communes-nc-limites-terrestres-simplifiees@nouvelle-caledonie/download/?format=shp&timezone=Europe/Berlin&lang=fr


annee_COG <- 2023

dossier_millesime_ADMINEXPRESS <- paste0(annee_COG,"/admin_express_cog/")
version_millesime_ADMINEXPRESS <- "ADECOG_3-1"


DEP_971 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGAF09UTM20_GLP/DEPARTEMENT.shp")) %>%  st_set_crs(5490) %>% select(INSEE_DEP)
DEP_972 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGAF09UTM20_MTQ/DEPARTEMENT.shp")) %>%   st_set_crs(5490) %>% select(INSEE_DEP)
DEP_973 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_UTM22RGFG95_GUF/DEPARTEMENT.shp")) %>%   st_set_crs(2972) %>% select(INSEE_DEP)
DEP_974 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGR92UTM40S_REU/DEPARTEMENT.shp")) %>%  st_set_crs(2975) %>% select(INSEE_DEP)
DEP_976 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGM04UTM38S_MYT/DEPARTEMENT.shp")) %>%  st_set_crs(4471) %>% select(INSEE_DEP)
DEP_977 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGAF09UTM20_STB-STM/DEPARTEMENT.shp")) %>%  st_set_crs(32620) %>% filter(INSEE_DEP %in% '977') %>% select(INSEE_DEP)
DEP_978 <-  st_read(paste0("U:/3-ressources/limites_admin/france/ign/",dossier_millesime_ADMINEXPRESS,version_millesime_ADMINEXPRESS,"_SHP_RGAF09UTM20_STB-STM/DEPARTEMENT.shp")) %>%  st_set_crs(32620) %>% filter(INSEE_DEP %in% '978') %>% select(INSEE_DEP)

DEP_975 <- st_read("https://static.data.gouv.fr/resources/decoupage-administratif-des-com-st-martin-et-st-barthelemy-et-com-saint-pierre-et-miquelon-format-admin-express/20220506-142220/departement.geojson",quiet = TRUE) %>%  select(INSEE_DEP)


dossier_limadmin_autres <- "U:/3-ressources/limites_admin/france/autres/"

# 986 : Wallis-et-Futuna
# - COMM
COMM_WF <- st_read(paste0(dossier_limadmin_autres,"wallis-et-futuna_0/communes_wallis_futuna.shp")) %>%
  filter(admin_leve %in% "8") %>%
  mutate(INSEE_COM = case_when(name %in% "Sigave" ~ "98612",
                               name %in% "Alo" ~ "98611",
                               TRUE ~ "98613" )) %>%
  ms_simplify(., keep = 0.05) %>%
  mutate(INSEE_DEP = "986") %>%
  select(CODGEO =INSEE_COM, LIBGEO = name, INSEE_DEP)
# - DEP
DEP_WF <- COMM_WF %>% group_by(INSEE_DEP) %>% summarise()

# 987 : Polynésie Française
# - COMM
COMM_PO <- st_read(paste0(dossier_limadmin_autres,"communes_polynesie/Com.shp")) %>%
  ms_simplify(., keep = 0.01) %>%
  st_transform(4326)  %>%
  mutate(INSEE_DEP = "987",
         INSEE_COM = paste0("987",IDCom)) %>%
  select(CODGEO=INSEE_COM , LIBGEO = Commune, INSEE_DEP) %>%
  st_make_valid()
# - DEP
DEP_PO <- COMM_PO %>% group_by(INSEE_DEP) %>% summarise()

# 988 : Nouvelle-Calédonie
# - COMM
COMM_NC <- st_read(paste0(dossier_limadmin_autres,"communes-nc-limites-terrestres-simplifiees/communes-nc-limites-terrestres-simplifiees.shp")) %>%
  ms_simplify(., keep = 0.02) %>%
  mutate(INSEE_DEP = "988") %>%
  select(CODGEO = code_com, LIBGEO = nom_minus, INSEE_DEP)
# - DEP
DEP_NC <- COMM_NC %>% group_by(INSEE_DEP) %>% summarise()





usethis::use_data(COMM_WF, overwrite = TRUE)
usethis::use_data(DEP_WF, overwrite = TRUE)

usethis::use_data(COMM_PO, overwrite = TRUE)
usethis::use_data(DEP_PO, overwrite = TRUE)

usethis::use_data(COMM_NC, overwrite = TRUE)
usethis::use_data(DEP_NC, overwrite = TRUE)



DEP_986 <- DEP_WF %>% select(INSEE_DEP)
DEP_987 <- DEP_PO %>% select(INSEE_DEP)
DEP_988 <- DEP_NC %>% select(INSEE_DEP)


usethis::use_data(DEP_971, overwrite = TRUE)
usethis::use_data(DEP_972, overwrite = TRUE)
usethis::use_data(DEP_973, overwrite = TRUE)
usethis::use_data(DEP_974, overwrite = TRUE)
usethis::use_data(DEP_975, overwrite = TRUE)
usethis::use_data(DEP_976, overwrite = TRUE)
usethis::use_data(DEP_977, overwrite = TRUE)
usethis::use_data(DEP_978, overwrite = TRUE)
usethis::use_data(DEP_986, overwrite = TRUE)
usethis::use_data(DEP_987, overwrite = TRUE)
usethis::use_data(DEP_988, overwrite = TRUE)
