library(tidyverse)
library(lubridate)


# 1. Chargement des données propres
df <- readRDS("data/final_data/data_clean.rds")

View(head(df))



trafic_jour <- df %>%
  group_by(JOUR) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE))


idf_color <- "#18458A"

ggplot(trafic_jour, aes(x = JOUR, y = Total)) +
  
  # 1. COURBE & TENDANCE
  geom_line(color = idf_color, alpha = 0.6, lwd = 0.4) +
  geom_smooth(method = "loess", color = "#D9303E", span = 0.05, se = FALSE, lwd = 1) +
  
  # 2. ANNOTATION COVID
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed", color = "grey50") +
  annotate("text", x = as.Date("2020-03-17"), y = 100000, 
           label = "Confinement", angle = 90, vjust = -1, size = 3, color = "grey30") +

  # 3. DESIGN
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text = element_text(color = "grey30")
  ) +
  
  # 4. AXES & TITRES (Mis à jour 2018-2024)
  labs(
    title = "Historique du trafic ferroviaire en Île-de-France",
    subtitle = "Évolution quotidienne des validations (2018 - 2024)",
    x = "", 
    y = "Voyageurs par jour",
    caption = "Source : IDFM Open Data"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  # 5. LE ZOOM (Arrêt strict au 31/12/2024)
  coord_cartesian(xlim = c(min(trafic_jour$JOUR), as.Date("2024-12-31")))




#2 
trafic_mensuel <- df %>%
  mutate(Mois = month(JOUR, label = TRUE, abbr = TRUE)) %>% # Crée les labels
  group_by(JOUR, Mois) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# 2. GRAPHIQUE
ggplot(trafic_mensuel, aes(x = Mois, y = Total, fill = Mois)) +
  
  # Le Boxplot : La boîte représente 50% des jours (la "normale")
  # La ligne au milieu de la boîte est la Médiane
  geom_boxplot(outlier.alpha = 0.2, outlier.color = "red", show.legend = FALSE) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Profil saisonnier du réseau (2018-2024)",
    subtitle = "Distribution du trafic quotidien par mois",
    x = "", 
    y = "Validations par jour"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_fill_viridis_d() # Une palette de couleurs lisible



# 3eme graphique : Trafic annuel total
trafic_annuel <- df %>%
  mutate(Annee = year(JOUR)) %>% # On extrait l'année (2018, 2019...)
  group_by(Annee) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE)) %>%
  # On filtre pour ne garder que les années complètes (2018-2024)
  filter(Annee <= 2024)

# 2. Le Graphique
ggplot(trafic_annuel, aes(x = factor(Annee), y = Total)) + # factor() enlève les décimales (2018.5)
  
  # On dessine les barres
  geom_col(fill = "steelblue", width = 0.7) +
  
  # On ajoute le chiffre exact au-dessus de chaque barre (pour la précision)
  geom_text(aes(label = round(Total / 1e6, 1)), vjust = -0.5, color = "black") +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Volume total des validations par an",
    subtitle = "Chiffres en Millions (M)",
    x = "Année",
    y = "Total Validations"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M"))





# --- TÂCHE 4 : COMPARATIF AVANCÉ (BENCHMARK) ---

# 1. PRÉPARATION DES DONNÉES JOURNALIÈRES
# On doit d'abord sommer toutes les gares pour avoir le total PAR JOUR
df_jour <- df %>%
  group_by(JOUR) %>%
  summarise(Total_Reseau = sum(NB_VALD, na.rm = TRUE)) %>%
  mutate(
    Annee = year(JOUR),
    Mois = month(JOUR),
    # On s'assure que Lundi est le 1er jour
    Jour_Semaine = wday(JOUR, label = TRUE, abbr = FALSE, week_start = 1) 
  )

# On enlève l'année 2020 pour les moyennes (car année atypique)
df_clean_stats <- df_jour %>% filter(Annee != 2020)


# 2. CRÉATION DES 4 PROFILS

# A. La Semaine Benchmark (Semaine spécifique du 04/03/2019 au 10/03/2019)
# C'est une semaine parfaite : pas de vacances, pas de grève, pré-Covid.
profil_benchmark <- df_jour %>%
  filter(JOUR >= as.Date("2019-03-04") & JOUR <= as.Date("2019-03-10")) %>%
  mutate(Type = "1. Benchmark (Mars 2019)") %>%
  select(Jour_Semaine, Valeur = Total_Reseau, Type)

# B. La Semaine "Chargée" (Travail)
# On prend tout SAUF l'été (Juillet-Août) et SAUF Noël (Décembre)
profil_travail <- df_clean_stats %>%
  filter(Mois %in% c(1, 2, 3, 11)) %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "2. Semaine Travail (Moyenne)")

# C. La Semaine "Vacances"
# On prend uniquement Juillet et Août
profil_vacances <- df_clean_stats %>%
  filter(Mois %in% c(7, 8)) %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "3. Semaine Vacances (Moyenne)")

# D. La Semaine "Moyenne" (Globale)
# Moyenne sur toute l'année (hors 2020)
profil_global <- df_clean_stats %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "4. Semaine Globale (Moyenne)")


# 3. ASSEMBLAGE ET GRAPHIQUE
# On colle les 4 tableaux l'un en dessous de l'autre
donnees_comparatives <- bind_rows(profil_benchmark, profil_travail, profil_vacances, profil_global)

# Le Graphique Final
ggplot(donnees_comparatives, aes(x = Jour_Semaine, y = Valeur, color = Type, group = Type)) +
  
  # Les Lignes
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  
  # Design
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparatif des profils hebdomadaires",
    subtitle = "Le Benchmark (Mars 2019) vs Les moyennes historiques (hors 2020)",
    x = "",
    y = "Validations par jour",
    color = "Profil"
  ) +
  
  # Couleurs personnalisées pour bien distinguer
  scale_color_manual(values = c(
    "1. Benchmark (Mars 2019)" = "black",      # Noir pour la référence absolue
    "2. Semaine Travail (Moyenne)" = "#D9303E", # Rouge pour le niveau haut
    "3. Semaine Vacances (Moyenne)" = "#2ECC71",# Vert pour les vacances
    "4. Semaine Globale (Moyenne)" = "steelblue" # Bleu pour la moyenne
  )) +
  
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
  theme(legend.position = "bottom") # Légende en bas pour plus de place