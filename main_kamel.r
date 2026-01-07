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
# write_csv(df, "data/final_data/validations_raw_2018_2024.csv")
