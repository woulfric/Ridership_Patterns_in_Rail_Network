library(tidyverse)
library(lubridate)
library(broom)

# 1. Chargement des données
df <- readRDS("data/final_data/data_clean.rds")

# 2. Préparation : On définit les périodes "Travail" vs "Vacances"
# On prend Janvier (Hors 1er janv) comme "Travail" et Août comme "Vacances"
df_stat <- df %>%
  mutate(
    Mois = month(JOUR),
    Annee = year(JOUR),
    Jour_Semaine = wday(JOUR, week_start = 1) # 1 = Lundi
  ) %>%
  # On filtre pour ne garder que Janvier et Août, et on enlève 2020
  filter(Mois %in% c(1, 8), Annee != 2020) %>%
  mutate(
    Periode = ifelse(Mois == 1, "Normal (Janvier)", "Vacances (Août)")
  )

# 3. Sélection du TOP 5 des Gares (pour ne pas faire le calcul sur 500 gares)
top_5_gares <- df_stat %>%
  group_by(NOM_GARE) %>%
  summarise(Total = sum(NB_VALD)) %>%
  slice_max(Total, n = 5) %>%
  pull(NOM_GARE)

print("--- Analyse sur le TOP 5 des Gares ---")
print(top_5_gares)

# On ne garde que ces 5 gares pour l'analyse
df_focus <- df_stat %>% filter(NOM_GARE %in% top_5_gares)


# ==============================================================================
# ETAPE 1 : STATISTIQUES DESCRIPTIVES (Moyennes & Écarts-Types)
# Comme demandé : "Calcul de moyennes et écarts-type par période"
# ==============================================================================

stats_descriptives <- df_focus %>%
  group_by(NOM_GARE, Periode) %>%
  summarise(
    Moyenne_Journaliere = mean(NB_VALD, na.rm = TRUE),
    Ecart_Type = sd(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Periode, values_from = c(Moyenne_Journaliere, Ecart_Type))

print("--- Tableau des Moyennes (à mettre dans le rapport) ---")
print(stats_descriptives)


# ==============================================================================
# ETAPE 2 : LE T-TEST (Validation Statistique)
# Comme demandé : "Applique un test de comparaison de moyennes (t-test)"
# ==============================================================================

print("--- Résultats des T-Tests par Gare ---")

# On applique le t-test pour chaque gare du Top 5
resultats_tests <- df_focus %>%
  group_by(NOM_GARE) %>%
  summarise(
    # On compare les validations en fonction de la période
    t_test = list(t.test(NB_VALD ~ Periode, data = cur_data())),
    .groups = "drop"
  ) %>%
  # On nettoie les résultats pour avoir un joli tableau
  mutate(res = map(t_test, broom::tidy)) %>%
  unnest(res) %>%
  select(NOM_GARE, p.value, estimate1, estimate2) %>%
  mutate(
    Significatif = ifelse(p.value < 0.05, "OUI", "NON"),
    # Calcul de la baisse en pourcentage pour l'interprétation
    Baisse_Pct = round(((estimate2 - estimate1) / estimate1) * 100, 1)
  )

# Affichage propre pour ton rapport
print(resultats_tests)

# Petit message d'aide à l'interprétation
print("INTERPRÉTATION :")
print("Si 'Significatif' est OUI, cela veut dire que la baisse observée n'est pas due au hasard.")
print("estimate1 = Moyenne Vacances, estimate2 = Moyenne Normale")