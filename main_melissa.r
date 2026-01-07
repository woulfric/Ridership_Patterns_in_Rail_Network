## -------------------------------
## 0. Préparation de l'environnement
## -------------------------------

# Chargement des principaux packages
library(tidyverse)   # dplyr, ggplot2, readr, etc.
library(lubridate)   # gestion des dates
library(data.table)  # fread si besoin
library(sf)          # données spatiales

# Vérifier le dossier de travail (projet RStudio)
getwd()
# Doit être : C:/Users/melis/OneDrive/Desktop/Cours/ADD- R/Projet-R-meli

# Vérifier la structure des dossiers de données
list.files()
list.files("data-rf-2018")
list.files("data-rf-2019")
list.files("data-rf-2020")
list.files("data-rf-2021")
list.files("data-rf-2022")
list.files("data-rf-2023")
list.files("data-rf-2024")
list.files("data")



## --------------------------------------------
## 1. Fonctions génériques de nettoyage/agrégat
## --------------------------------------------

# 1.1 Nettoyage des fichiers historiques (2018–2024)
#    - homogénéise types
#    - garde les colonnes utiles
clean_valid_hist <- function(df) {
  df %>%
    mutate(
      JOUR            = as.Date(JOUR, format = "%d/%m/%Y"),
      CODE_STIF_ARRET = as.character(CODE_STIF_ARRET),
      NB_VALD         = as.numeric(NB_VALD)
    ) %>%
    select(JOUR, CODE_STIF_ARRET, NB_VALD)
}

# 1.2 Agrégation par jour et par arrêt
agg_by_day_stop <- function(df) {
  df %>%
    group_by(JOUR, CODE_STIF_ARRET) %>%
    summarise(
      NB_VALD = sum(NB_VALD, na.rm = TRUE),
      .groups = "drop"
    )
}



## ----------------------------------------
## 2. Construction de la base 2018–2024
## ----------------------------------------

### 2.1. Exemple détaillé 2018

# 2018 S1
fer_2018_s1 <- readr::read_table(
  "data-rf-2018/2018_S1_NB_FER.txt",
  col_names = TRUE
)
valid_2018_s1 <- clean_valid_hist(fer_2018_s1)
valid_2018_s1_agg <- agg_by_day_stop(valid_2018_s1)

# 2018 S2
fer_2018_s2 <- readr::read_table(
  "data-rf-2018/2018_S2_NB_FER.txt",
  col_names = TRUE
)
valid_2018_s2 <- clean_valid_hist(fer_2018_s2)
valid_2018_s2_agg <- agg_by_day_stop(valid_2018_s2)

# 2018 complet
valid_2018 <- bind_rows(valid_2018_s1_agg, valid_2018_s2_agg)


### 2.2. 2019

fer_2019_s1 <- readr::read_table(
  "data-rf-2019/2019_S1_NB_FER.txt",
  col_names = TRUE
)
fer_2019_s2 <- readr::read_table(
  "data-rf-2019/2019_S2_NB_FER.txt",
  col_names = TRUE
)

valid_2019_s1 <- clean_valid_hist(fer_2019_s1)
valid_2019_s2 <- clean_valid_hist(fer_2019_s2)

valid_2019_s1_agg <- agg_by_day_stop(valid_2019_s1)
valid_2019_s2_agg <- agg_by_day_stop(valid_2019_s2)

valid_2019 <- bind_rows(valid_2019_s1_agg, valid_2019_s2_agg)


### 2.3. 2020

fer_2020_s1 <- readr::read_table(
  "data-rf-2020/2020_S1_NB_FER.txt",
  col_names = TRUE
)
fer_2020_s2 <- readr::read_table(
  "data-rf-2020/2020_S2_NB_FER.txt",
  col_names = TRUE
)

valid_2020_s1 <- clean_valid_hist(fer_2020_s1)
valid_2020_s2 <- clean_valid_hist(fer_2020_s2)

valid_2020_s1_agg <- agg_by_day_stop(valid_2020_s1)
valid_2020_s2_agg <- agg_by_day_stop(valid_2020_s2)

valid_2020 <- bind_rows(valid_2020_s1_agg, valid_2020_s2_agg)


### 2.4. 2021

fer_2021_s1 <- readr::read_table(
  "data-rf-2021/2021_S1_NB_FER.txt",
  col_names = TRUE
)
fer_2021_s2 <- readr::read_table(
  "data-rf-2021/2021_S2_NB_FER.txt",
  col_names = TRUE
)

valid_2021_s1 <- clean_valid_hist(fer_2021_s1)
valid_2021_s2 <- clean_valid_hist(fer_2021_s2)

valid_2021_s1_agg <- agg_by_day_stop(valid_2021_s1)
valid_2021_s2_agg <- agg_by_day_stop(valid_2021_s2)

valid_2021 <- bind_rows(valid_2021_s1_agg, valid_2021_s2_agg)


### 2.5. 2022 (cas mixte : S1 fichier un peu sale, S2 bien délimité)

fer_2022_s1 <- readr::read_table(
  "data-rf-2022/2022_S1_NB_FER.txt",
  col_names = TRUE
)
fer_2022_s2 <- readr::read_delim(
  "data-rf-2022/2022_S2_NB_FER.txt",
  delim = ";"
)

valid_2022_s1 <- clean_valid_hist(fer_2022_s1)
valid_2022_s2 <- clean_valid_hist(fer_2022_s2)

valid_2022_s1_agg <- agg_by_day_stop(valid_2022_s1)
valid_2022_s2_agg <- agg_by_day_stop(valid_2022_s2)

valid_2022 <- bind_rows(valid_2022_s1_agg, valid_2022_s2_agg)


### 2.6. 2023 (S1 encodage particulier, S2 UTF‑16 + tabulation)

# S1 (attention à l'espace dans le nom de fichier)
fer_2023_s1 <- readr::read_table(
  "data-rf-2023/2023_S1_NB_FER .txt",
  col_names = TRUE
)

# S2 en UTF‑16LE et séparateur tab
fer_2023_s2 <- read.delim(
  "data-rf-2023/2023_S2_NB_FER.txt",
  fileEncoding     = "UTF-16LE",
  sep              = "\t",
  header           = TRUE,
  stringsAsFactors = FALSE
)

valid_2023_s1 <- clean_valid_hist(fer_2023_s1)
valid_2023_s2 <- clean_valid_hist(fer_2023_s2)

valid_2023_s1_agg <- agg_by_day_stop(valid_2023_s1)
valid_2023_s2_agg <- agg_by_day_stop(valid_2023_s2)

valid_2023 <- bind_rows(valid_2023_s1_agg, valid_2023_s2_agg)


### 2.7. 2024 (S1 et T3)

fer_2024_s1 <- readr::read_table(
  "data-rf-2024/2024_S1_NB_FER.txt",
  col_names = TRUE
)

fer_2024_t3_raw <- readr::read_delim(
  "data-rf-2024/2024_T3_NB_FER.txt",
  col_names = TRUE
)

# Correction de l'année dans les dates T3 (format xx/xx/24)
fer_2024_t3 <- fer_2024_t3_raw %>%
  mutate(
    JOUR = stringr::str_replace(JOUR, "/24$", "/2024"),
    JOUR = dmy(JOUR)
  )

valid_2024_s1 <- clean_valid_hist(fer_2024_s1)
valid_2024_t3 <- clean_valid_hist(fer_2024_t3)

valid_2024_s1_agg <- agg_by_day_stop(valid_2024_s1)
valid_2024_t3_agg <- agg_by_day_stop(valid_2024_t3)

valid_2024 <- bind_rows(valid_2024_s1_agg, valid_2024_t3_agg)



## -----------------------------
## 3. Chargement des données 2025
## -----------------------------

# Tu avais déjà créé ce fichier dans ton script cleaning.R
valid_2025 <- readRDS("data/validations_2025_clean.rds")
# Structure attendue : JOUR (Date), CODE_STIF_ARRET (chr), NB_VALD (num)



## ---------------------------------------------
## 4. Base consolidée 2018–2025 (valid_all)
## ---------------------------------------------

valid_hist_all <- bind_rows(
  valid_2018,
  valid_2019,
  valid_2020,
  valid_2021,
  valid_2022,
  valid_2023,
  valid_2024
) %>%
  filter(JOUR >= as.Date("2018-01-01"))

valid_all <- bind_rows(
  valid_hist_all,
  valid_2025
) %>%
  filter(JOUR >= as.Date("2018-01-01")) %>%
  mutate(
    annee        = year(JOUR),
    mois         = month(JOUR, label = TRUE, abbr = TRUE),
    semaine_iso  = isoweek(JOUR),
    jour_semaine = wday(JOUR, label = TRUE, abbr = TRUE, week_start = 1)
  )

# Check global
dim(valid_all)
head(valid_all)


## -------------------------------------------------
## 5. Semaine de référence + périodes de vacances
## -------------------------------------------------

# 5.1 Semaine "normale" 2019 (référence)
semaine_ref <- valid_all %>%
  filter(
    annee == 2019,
    JOUR >= as.Date("2019-04-08"),
    JOUR <= as.Date("2019-04-14")
  ) %>%
  group_by(CODE_STIF_ARRET, jour_semaine) %>%
  summarise(
    NB_VALD_ref = mean(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

# 5.2 Vacances d'été 2024 (exemple)
periode_ete_2024 <- valid_all %>%
  filter(
    JOUR >= as.Date("2024-08-01"),
    JOUR <= as.Date("2024-08-31")
  ) %>%
  group_by(CODE_STIF_ARRET, jour_semaine) %>%
  summarise(
    NB_VALD_cmp = mean(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

comparaison_ete <- semaine_ref %>%
  inner_join(periode_ete_2024,
             by = c("CODE_STIF_ARRET", "jour_semaine")) %>%
  mutate(
    diff_abs = NB_VALD_cmp - NB_VALD_ref,
    diff_rel = 100 * diff_abs / NB_VALD_ref
  )

# 5.3 Vacances de printemps 2019 (celles que tu as utilisées)
vacances_printemps_2019 <- valid_all %>%
  filter(
    JOUR >= as.Date("2019-04-20"),
    JOUR <= as.Date("2019-05-05")   # veille de la reprise
  ) %>%
  group_by(CODE_STIF_ARRET, jour_semaine) %>%
  summarise(
    NB_VALD_cmp = mean(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

comparaison_printemps <- semaine_ref %>%
  inner_join(vacances_printemps_2019,
             by = c("CODE_STIF_ARRET", "jour_semaine")) %>%
  mutate(
    diff_abs = NB_VALD_cmp - NB_VALD_ref,
    diff_rel = 100 * diff_abs / NB_VALD_ref
  )


## ----------------------------------------
## 6. Histogramme et top 20 des écarts
## ----------------------------------------

# Distribution globale des écarts relatifs
ggplot(comparaison_printemps, aes(x = diff_rel)) +
  geom_histogram(bins = 50) +
  labs(
    x = "Écart relatif (%)",
    y = "Nombre de zones d'arrêt",
    title = "Distribution des écarts de fréquentation vacances vs semaine normale"
  )

# Top 20 plus forte baisse
top_baisse <- comparaison_printemps %>%
  arrange(diff_rel) %>%
  slice_head(n = 20)

# Top 20 plus forte hausse
top_hausse <- comparaison_printemps %>%
  arrange(desc(diff_rel)) %>%
  slice_head(n = 20)

# Graphique top 20 baisse (par ID d'arrêt)
ggplot(top_baisse,
       aes(x = reorder(CODE_STIF_ARRET, diff_rel), y = diff_rel)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Arrêt (CODE_STIF_ARRET)",
    y = "Écart relatif (%)",
    title = "Top 20 des arrêts en plus forte baisse (vacances de printemps 2019)"
  )


## --------------------------------------------------------
## 7. Référentiel zones d'arrêt + carte des écarts
## --------------------------------------------------------

# 7.1 Charger le shapefile IdFM des zones d'arrêt
zda_sf <- st_read("REF_ZdA/PL_ZDL_R_02-01-2026.shp")

# idrefa_lda = identifiant de la zone d'arrêt (numérique)
# nom_lda    = libellé de la zone d'arrêt
names(zda_sf)

# 7.2 Créer un mapping CODE_STIF_ARRET -> ID_ZDC
#     On le construit à partir d'un fichier brut contenant ID_ZDC (ex : fer_2023_s2)
id_mapping <- fer_2023_s2 %>%
  select(CODE_STIF_ARRET, ID_ZDC) %>%
  distinct() %>%
  filter(!is.na(ID_ZDC)) %>%
  mutate(
    CODE_STIF_ARRET = as.character(CODE_STIF_ARRET),
    ID_ZDC          = as.character(ID_ZDC)
  )

# 7.3 Préparer le shapefile avec idrefa_lda en character
zda_sf_chr <- zda_sf %>%
  mutate(idrefa_lda = as.character(idrefa_lda))

# 7.4 Ajouter ID_ZDC dans la table de comparaison
comparaison_idzdc <- comparaison_printemps %>%
  mutate(CODE_STIF_ARRET = as.character(CODE_STIF_ARRET)) %>%
  left_join(id_mapping, by = "CODE_STIF_ARRET")

# 7.5 Jointure avec le référentiel géographique
comparaison_sf <- comparaison_idzdc %>%
  left_join(
    zda_sf_chr,
    by = c("ID_ZDC" = "idrefa_lda"),
    relationship = "many-to-many"   # accepté ici
  ) %>%
  st_as_sf()

# Contrôles
st_crs(comparaison_sf)              # devrait être 2154 (Lambert-93)
nrow(comparaison_sf)
sum(st_is_empty(comparaison_sf))    # nombre de géométries vides





ggplot(comparaison_sf) +
  geom_sf(color = "black", fill = NA, size = 0.1) +
  coord_sf(default_crs = NULL) +
  labs(title = "Zones d'arrêt jointes (test visuel)")






# Tronquer les valeurs extrêmes de diff_rel pour la palette
comparaison_sf_plot <- comparaison_sf %>%
  mutate(
    diff_rel_cap = pmax(pmin(diff_rel, 100), -100)  # entre -100% et +100%
  )

ggplot(comparaison_sf_plot) +
  geom_sf(aes(fill = diff_rel_cap), color = NA, size = 0.05) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Écart relatif (%)"
  ) +
  coord_sf(default_crs = st_crs(comparaison_sf)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "Écart de fréquentation vacances vs semaine normale\n(vacances de printemps 2019, tronqué entre -100% et +100%)"
  )






#Carte choroplèthe des écarts
ggplot(comparaison_sf_plot) +
  geom_sf(aes(fill = diff_rel_cap), color = NA, size = 0.05) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Écart relatif (%)"
  ) +
  coord_sf(
    xlim = c(640000, 710000),
    ylim = c(6840000, 6925000),
    default_crs = sf::st_crs(comparaison_sf)
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(
    title = "Écart de fréquentation vacances vs semaine normale\n(vacances de printemps 2019)",
    caption = "Écarts relatifs tronqués entre -100% et +100% pour la lisibilité"
  )









#top20:
top_baisse_nom <- comparaison_sf %>%
  arrange(diff_rel) %>%
  slice_head(n = 20)

ggplot(top_baisse_nom,
       aes(x = reorder(nom_lda, diff_rel), y = diff_rel)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Gare / zone d'arrêt",
    y = "Écart relatif (%)",
    title = "Top 20 des gares en plus forte baisse (vacances de printemps 2019)"
  )









top_hausse_nom <- comparaison_sf %>%
  arrange(desc(diff_rel)) %>%
  slice_head(n = 20)

ggplot(top_hausse_nom,
       aes(x = reorder(nom_lda, diff_rel), y = diff_rel)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Gare / zone d'arrêt",
    y = "Écart relatif (%)",
    title = "Top 20 des gares en plus forte hausse (vacances de printemps 2019)"
  )













top_baisse_table <- top_baisse_nom %>%
  select(
    CODE_STIF_ARRET,
    nom_lda,
    NB_VALD_ref,
    NB_VALD_cmp,
    diff_rel
  )

top_baisse_table


















periode_ete_2024 <- valid_all %>%
  filter(
    JOUR >= as.Date("2024-08-01"),
    JOUR <= as.Date("2024-08-31")
  ) %>%
  group_by(CODE_STIF_ARRET, jour_semaine) %>%
  summarise(
    NB_VALD_cmp = mean(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

comparaison_ete <- semaine_ref %>%
  inner_join(periode_ete_2024,
             by = c("CODE_STIF_ARRET", "jour_semaine")) %>%
  mutate(
    diff_abs = NB_VALD_cmp - NB_VALD_ref,
    diff_rel = 100 * diff_abs / NB_VALD_ref
  )

# Reprendre exactement les mêmes étapes que pour comparaison_printemps :
# - jointure avec id_mapping + zda_sf_chr
# - création d'un comparaison_ete_sf
# - carte + top 20



















## Fonction générique : compare une période [date_debut, date_fin] à la semaine de ref 2019
compare_periode <- function(data_all, date_debut, date_fin, semaine_ref) {
  periode_cmp <- data_all %>%
    filter(JOUR >= as.Date(date_debut),
           JOUR <= as.Date(date_fin)) %>%
    group_by(CODE_STIF_ARRET, jour_semaine) %>%
    summarise(
      NB_VALD_cmp = mean(NB_VALD, na.rm = TRUE),
      .groups = "drop"
    )
  
  comparaison <- semaine_ref %>%
    inner_join(periode_cmp,
               by = c("CODE_STIF_ARRET", "jour_semaine")) %>%
    mutate(
      diff_abs = NB_VALD_cmp - NB_VALD_ref,
      diff_rel = 100 * diff_abs / NB_VALD_ref
    )
  
  comparaison
}









comparaison_printemps <- compare_periode(
  valid_all, "2019-04-20", "2019-05-05", semaine_ref
)

comparaison_ete_2024 <- compare_periode(
  valid_all, "2024-08-01", "2024-08-31", semaine_ref
)

















## mapping CODE_STIF_ARRET -> ID_ZDC (calculé une fois)
id_mapping <- fer_2023_s2 %>%
  select(CODE_STIF_ARRET, ID_ZDC) %>%
  distinct() %>%
  filter(!is.na(ID_ZDC)) %>%
  mutate(
    CODE_STIF_ARRET = as.character(CODE_STIF_ARRET),
    ID_ZDC          = as.character(ID_ZDC)
  )

## shapefile zones d'arrêt, préparé une fois
zda_sf <- st_read("REF_ZdA/PL_ZDL_R_02-01-2026.shp")
zda_sf_chr <- zda_sf %>%
  mutate(idrefa_lda = as.character(idrefa_lda))

## Fonction : ajoute la géométrie et les noms aux comparaisons
add_zones_geometry <- function(comparaison_tab, id_mapping, zda_sf_chr) {
  comparaison_idzdc <- comparaison_tab %>%
    mutate(CODE_STIF_ARRET = as.character(CODE_STIF_ARRET)) %>%
    left_join(id_mapping, by = "CODE_STIF_ARRET")
  
  comparaison_sf <- comparaison_idzdc %>%
    left_join(
      zda_sf_chr,
      by = c("ID_ZDC" = "idrefa_lda"),
      relationship = "many-to-many"
    ) %>%
    st_as_sf()
  
  comparaison_sf
}






comparaison_printemps_sf <- add_zones_geometry(
  comparaison_printemps, id_mapping, zda_sf_chr
)

comparaison_ete_2024_sf <- add_zones_geometry(
  comparaison_ete_2024, id_mapping, zda_sf_chr
)







plot_carte_ecarts <- function(comp_sf, titre) {
  comp_plot <- comp_sf %>%
    mutate(diff_rel_cap = pmax(pmin(diff_rel, 100), -100))
  
  ggplot(comp_plot) +
    geom_sf(aes(fill = diff_rel_cap), color = NA, size = 0.05) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Écart relatif (%)"
    ) +
    coord_sf(
      xlim = c(640000, 710000),
      ylim = c(6840000, 6925000),
      default_crs = st_crs(comp_sf)
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    labs(
      title   = titre,
      caption = "Écarts relatifs tronqués entre -100% et +100% pour la lisibilité"
    )
}



plot_carte_ecarts(
  comparaison_printemps_sf,
  "Écart vacances de printemps 2019 vs semaine normale 2019"
)

plot_carte_ecarts(
  comparaison_ete_2024_sf,
  "Écart vacances d'été 2024 vs semaine normale 2019"
)


