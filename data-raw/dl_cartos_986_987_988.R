## code to prepare `dl_cartos_986_987_988` dataset goes here
library(dplyr)
library(sf)
library(rmapshaper)

# Récupération des contours communaux des collectivités d'outre-mer suivantes :
# 986 : https://nauru-data.sprep.org/system/files/wallis-et-futuna_0.zip
# 987 : https://static.data.gouv.fr/resources/limites-geographiques-administratives/20220610-202135/shapefiles.zip
# 988 : https://data.opendatasoft.com/explore/dataset/communes-nc-limites-terrestres-simplifiees@nouvelle-caledonie/download/?format=shp&timezone=Europe/Berlin&lang=fr


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
