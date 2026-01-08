library(tidyverse)

# Ouverture des fichiers CSV

# validation_1_trim_2025 = read.csv("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv", sep = ";")
# validation_2_trim_2025 = read.csv("data/validations-reseau-ferre-nombre-validations-par-jour-2eme-trimestre.csv", sep = ";")

# View(head(validation_1_trim_2025))
# View(head(validation_2_trim_2025))


#Ouverture des fichier txt
#2018
validation_1_trim_2018 = read.delim("data/data-rf-2018/2018_S1_NB_FER.txt")
validation_2_trim_2018 = read.delim("data/data-rf-2018/2018_S2_NB_FER.txt")

#2019
validation_1_trim_2019 = read.delim("data/data-rf-2019/2019_S1_NB_FER.txt")
validation_2_trim_2019 = read.delim("data/data-rf-2019/2019_S2_NB_FER.txt")

#2020
validation_1_trim_2020 = read.delim("data/data-rf-2020/2020_S1_NB_FER.txt")
validation_2_trim_2020 = read.delim("data/data-rf-2020/2020_S2_NB_FER.txt")

#2021
validation_1_trim_2021 = read.delim("data/data-rf-2021/2021_S1_NB_FER.txt")
validation_2_trim_2021 = read.delim("data/data-rf-2021/2021_S2_NB_FER.txt")

#2022
validation_1_trim_2022 = read.delim("data/data-rf-2022/2022_S1_NB_FER.txt")
#on ajoute un separateur car il est different du reste des fichiers
validation_2_trim_2022 = read.delim("data/data-rf-2022/2022_S2_NB_FER.txt", sep = ";")

#2023
validation_1_trim_2023 = read.delim("data/data-rf-2023/2023_S1_NB_FER .txt")
validation_2_trim_2023 = read.delim("data/data-rf-2023/2023_S2_NB_FER.txt", fileEncoding = "UTF-16LE")

#2024
validation_1_trim_2024 = read.delim("data/data-rf-2024/2024_S1_NB_FER.txt")
validation_2_trim_2024 = read.delim("data/data-rf-2024/2024_T3_NB_FER.txt")



# 1. On met tes variables dans une liste
liste_dfs <- list(
  validation_1_trim_2018, validation_2_trim_2018,
  validation_1_trim_2019, validation_2_trim_2019,
  validation_1_trim_2020, validation_2_trim_2020,
  validation_1_trim_2021, validation_2_trim_2021,
  validation_1_trim_2022, validation_2_trim_2022,
  validation_1_trim_2023, validation_2_trim_2023,
  validation_1_trim_2024, validation_2_trim_2024
)

for (i in 1:length(liste_dfs)) {
  
  print(paste("--- FICHIER N°", i, "---"))
  
  # Affiche le type de chaque colonne
  print(sapply(liste_dfs[[i]], class))
  
}


clean_valid_type <- function(df) {
  df %>%
    # --- CORRECTION ICI ---
    # On parcourt TOUS les noms de colonnes (.)
    # Si le nom est dans la liste des intrus, on le change. Sinon on garde l'original.
    rename_with(~ ifelse(. %in% c("lda", "ID_ZDC"), "ID_REFA_LDA", .)) %>%
    
    # --- ETAPE 2 : Types ---
    mutate(
      CODE_STIF_RES   = as.character(CODE_STIF_RES),
      CODE_STIF_ARRET = as.character(CODE_STIF_ARRET),
      NB_VALD         = as.integer(NB_VALD),
      JOUR            = as.Date(parse_date_time(JOUR, orders = c("ymd", "dmy")))
    )
}

liste_dfs_clean <- map(liste_dfs, clean_valid_type)

for (i in 1:length(liste_dfs_clean)) {
  
  print(paste("--- FICHIER N°", i, "---"))
  
  # Affiche le type de chaque colonne
  print(sapply(liste_dfs_clean[[i]], class))
  
}

df <- bind_rows(liste_dfs_clean)
View(head(validation_2_trim_2022))

# Export du fichier avant nettoyage
if (!dir.exists("data/final_data")) {
  dir.create("data/final_data")
}
write_csv(df, "data/final_data/validations_raw_2018_2024.csv")



#________________________nettoyage des données________________________


print("--- Valeurs manquantes par colonne ---")
print(colSums(is.na(df)))

# dico_arrets <- df %>%
#   # On sélectionne les 3 colonnes d'intérêt
#   select(LIBELLE_ARRET, CODE_STIF_ARRET, CODE_STIF_RES) %>%
  
#   # On nettoie les vides
#   filter(
#     !is.na(LIBELLE_ARRET) & LIBELLE_ARRET != "",
#     !is.na(CODE_STIF_ARRET) & CODE_STIF_ARRET != "",
#     !is.na(CODE_STIF_RES) & CODE_STIF_RES != ""
#   ) %>%
#   # On ne garde que les combinaisons uniques
#   distinct() %>%

#   # On trie par nom de gare pour que ce soit propre
#   arrange(LIBELLE_ARRET, CODE_STIF_RES, CODE_STIF_ARRET)

# View(dico_arrets)


# #pour afficher tout les arret avec leur code stif et reseau
# vision_groupee <- dico_arrets %>%
#   group_by(LIBELLE_ARRET) %>%
#   summarise(
#     Liste_Codes_Arret = paste(unique(CODE_STIF_ARRET), collapse = ", "),
#     Liste_Reseaux     = paste(unique(CODE_STIF_RES), collapse = ", "),
#     Nombre_Codes      = n()
#   ) %>%
#   arrange(desc(Nombre_Codes)) # Affiche en premier les gares qui ont le plus de codes

# View(vision_groupee)



# On isole les lignes qui posent problème
lignes_a_probleme <- df %>%
  filter(
    is.na(CODE_STIF_ARRET) | CODE_STIF_ARRET == "" |  # Code Arrêt manquant
    is.na(CODE_STIF_RES)   | CODE_STIF_RES == ""      # OU Code Réseau manquant
  )

# On regarde combien il y en a
View(lignes_a_probleme)


# On enlève les lignes qui posent problème
nb_avant <- nrow(df)
df <- df %>%
  filter(LIBELLE_ARRET != "Inconnu")

# Bilan
nb_apres <- nrow(df)
print(paste("Lignes supprimées :", nb_avant - nb_apres))
print(paste("Lignes restantes  :", nb_apres))

print("--- Valeurs manquantes par colonne ---")
print(colSums(is.na(df)))


#on gere la colone type de titre
df <- df %>%
  mutate(CATEGORIE_TITRE = case_match(CATEGORIE_TITRE,
    
    # 1. IMAGINE R
    c("IMAGINE R", "Imagine R") ~ "Imagine R",
    
    # 2. FORFAIT NAVIGO
    c("NAVIGO", "Forfait Navigo") ~ "Forfait Navigo",
    
    # 3. CONTRAT SOLIDARITE (TST + bug encodage)
    c("TST", "Contrat Solidarité Transport", "Contrat Solidarit\xe9 Transport") ~ "Contrat Solidarité Transport",
    
    # 4. AMETHYSTE
    c("AMETHYSTE", "Amethyste") ~ "Amethyste",
    
    # 5. FORFAITS COURTS
    c("NAVIGO JOUR", "Forfaits courts") ~ "Forfaits courts",
    
    # 6. FGT Forfait Gratuité Transport
    "FGT" ~ "Forfait Gratuité Transport",
    
    # 7. NON DEFINI
    c("?", "NON DEFINI") ~ "NON DEFINI",
    
    # 8. TOUT LE RESTE -> Autres titres
    .default = "Autres titres"
  ))

# --- VÉRIFICATION ---
print("--- Nouvelles catégories (avec FGT séparé) ---")
df %>% count(CATEGORIE_TITRE, sort = TRUE)

# Affichage des catégories distinctes
print(df %>% distinct(CATEGORIE_TITRE))


# Analyse de la colonne NB_VALD

lignes_na_validations <- df %>%
  filter(is.na(NB_VALD))

View(lignes_na_validations)

print("--- Résumé statistique avant nettoyage ---")
summary(df$NB_VALD)

print(paste("Nombre de valeurs manquantes (NA) :", sum(is.na(df$NB_VALD))))

# On regarde s'il y a des valeurs négatives (impossible logiquement)
print(paste("Nombre de valeurs négatives :", sum(df$NB_VALD < 0, na.rm = TRUE)))


n_avant <- nrow(df)
df <- df %>%

  # On supprime tout ce qui est NA (les anciens textes + les vides)
  filter(!is.na(NB_VALD))

# Bilan
n_apres <- nrow(df)
print(paste("Lignes supprimées :", n_avant - n_apres))
print(paste("Lignes restantes :", n_apres))


# --- VÉRIFICATION FINALE --- on enleve les date inferieure à 2018-01-01
df <- df %>%
  filter(JOUR >= as.Date("2018-01-01") & JOUR <= as.Date("2024-12-31"))

nrow(df)

# Export du fichier apres nettoyage
if (!dir.exists("data/final_data")) {
  dir.create("data/final_data")
}
write_csv(df, "data/final_data/validations_clean_2018_2024.csv")

# _____________________Agregation finale_______________________
library(sf)

# On joint les données géographiques au dataframe principal
df_agg <- df %>%
  # On groupe par date, par lieu (ZdA) et par type de titre
  group_by(JOUR, ID_REFA_LDA, CATEGORIE_TITRE) %>%
  
  # On somme les validations pour réduire le nombre de lignes
  summarise(
    NB_VALD = sum(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

View(head(df_agg))




# # Import des données géographiques des zones d'arrêts
# geo = read.csv("data/zones-d-arrets.csv", sep = ";")
# View(head(geo, 100))

# colnames(geo)

# geo_clean <- geo %>%
#   select(
#     ID_REFA_LDA = ZdAId,      # On renomme ZdAId -> ID_REFA_LDA
#     NOM_GARE = ZdAName,       # On renomme pour que ce soit parlant
#     VILLE = ZdATown,
#     X_L93 = ZdAXEpsg2154,     # Coordonnée X (Lambert 93)
#     Y_L93 = ZdAYEpsg2154      # Coordonnée Y (Lambert 93)
#   )

# # Jointure finale
# df_final <- df_agg %>% left_join(geo_clean, by = "ID_REFA_LDA")
# View(head(df_final))


geo_shp <- st_read("data/geo/PL_ZDL_R_07-01-2026.shp")
View(head(geo_shp))
colnames(geo_shp)

geo_clean <- geo_shp %>%
  st_transform(4326) %>%      # Conversion en format GPS standard (WGS84)
  st_centroid() %>%           # On prend le point central de la zone
  mutate(
    # On récupère les coordonnées X et Y
    LON = st_coordinates(.)[,1],
    LAT = st_coordinates(.)[,2],
    # On s'assure que l'ID est numérique pour la jointure
    ID_REFA_LDA = as.numeric(idrefa_lda)
  ) %>%
  # On garde uniquement les infos utiles et on enlève la géométrie lourde
  st_drop_geometry() %>%
  select(ID_REFA_LDA, NOM_GARE = nom_lda, VILLE = commune, LON, LAT) %>%
  # On supprime les éventuels doublons de zone
  distinct(ID_REFA_LDA, .keep_all = TRUE)

df_final <- df_agg %>%
  ungroup() %>%
  left_join(geo_clean, by = "ID_REFA_LDA")


View(head(df_final))

# Export du fichier apres nettoyage
if (!dir.exists("data/final_data")) {
  dir.create("data/final_data")
}
write_csv(df, "data/final_data/validations_geo_2018_2024.csv")
saveRDS(df_final, "data/final_data/data_clean.rds", compress = "xz")


df <- readRDS("data/final_data/data_clean.rds")
View(head(df))

df_random <- df %>% slice_sample(n = 100000)

write_csv(df_random, "data/final_data/validations_geo_2018_2024_sample100k.csv")
