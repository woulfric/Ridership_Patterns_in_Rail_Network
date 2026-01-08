library(tidyverse)
library(lubridate)
library(scales)

# 0. CRÉATION DU DOSSIER POUR LES IMAGES
if(!dir.exists("img")) dir.create("img")

# 1. CHARGEMENT DES DONNÉES
df <- readRDS("data/final_data/data_clean.rds")

# ==============================================================================
# GRAPHIQUE 1 : HISTORIQUE QUOTIDIEN (TIME SERIES)
# ==============================================================================

trafic_jour <- df %>%
  group_by(JOUR) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE))

idf_color <- "#18458A"

# On stocke le graph dans 'p1'
p1 <- ggplot(trafic_jour, aes(x = JOUR, y = Total)) +
  
  # COURBE & TENDANCE
  geom_line(color = idf_color, alpha = 0.6, lwd = 0.4) +
  geom_smooth(method = "loess", color = "#D9303E", span = 0.05, se = FALSE, lwd = 1) +
  
  # ANNOTATION COVID
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed", color = "grey50") +
  annotate("text", x = as.Date("2020-03-17"), y = 100000, 
           label = "Confinement", angle = 90, vjust = -1, size = 3, color = "grey30") +

  # DESIGN
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    axis.text = element_text(color = "grey30")
  ) +
  
  # AXES & TITRES
  labs(
    title = "Historique du trafic ferroviaire en Île-de-France",
    subtitle = "Évolution quotidienne des validations (2018 - 2024)",
    x = "", 
    y = "Voyageurs par jour",
    caption = "Source : IDFM Open Data"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  # LE ZOOM
  coord_cartesian(xlim = c(min(trafic_jour$JOUR), as.Date("2024-12-31")))

# Affichage et Sauvegarde
print(p1)
ggsave("img/1_historique_trafic.png", plot = p1, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 2 : PROFIL SAISONNIER (BOXPLOT)
# ==============================================================================

trafic_mensuel <- df %>%
  mutate(Mois = month(JOUR, label = TRUE, abbr = TRUE)) %>%
  group_by(JOUR, Mois) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# On stocke dans 'p2'
p2 <- ggplot(trafic_mensuel, aes(x = Mois, y = Total, fill = Mois)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.color = "red", show.legend = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Profil saisonnier du réseau (2018-2024)",
    subtitle = "Distribution du trafic quotidien par mois",
    x = "", 
    y = "Validations par jour"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_fill_viridis_d()

# Affichage et Sauvegarde
print(p2)
ggsave("img/2_saisonnalite_mensuelle.png", plot = p2, width = 10, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 3 : TRAFIC ANNUEL (BARPLOT)
# ==============================================================================

trafic_annuel <- df %>%
  mutate(Annee = year(JOUR)) %>%
  group_by(Annee) %>%
  summarise(Total = sum(NB_VALD, na.rm = TRUE)) %>%
  filter(Annee <= 2024)

# On stocke dans 'p3'
p3 <- ggplot(trafic_annuel, aes(x = factor(Annee), y = Total)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = round(Total / 1e6, 1)), vjust = -0.5, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Volume total des validations par an",
    subtitle = "Chiffres en Millions (M)",
    x = "Année",
    y = "Total Validations"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M"))

# Affichage et Sauvegarde
print(p3)
ggsave("img/3_trafic_annuel.png", plot = p3, width = 8, height = 6, dpi = 300)


# ==============================================================================
# GRAPHIQUE 4 : COMPARATIF BENCHMARK (LINE CHART)
# ==============================================================================

# Préparation
df_jour <- df %>%
  group_by(JOUR) %>%
  summarise(Total_Reseau = sum(NB_VALD, na.rm = TRUE)) %>%
  mutate(
    Annee = year(JOUR),
    Mois = month(JOUR),
    Jour_Semaine = wday(JOUR, label = TRUE, abbr = FALSE, week_start = 1) 
  )

df_clean_stats <- df_jour %>% filter(Annee != 2020)

# Création des profils
profil_benchmark <- df_jour %>%
  filter(JOUR >= as.Date("2019-03-04") & JOUR <= as.Date("2019-03-10")) %>%
  mutate(Type = "1. Benchmark (Mars 2019)") %>%
  select(Jour_Semaine, Valeur = Total_Reseau, Type)

profil_travail <- df_clean_stats %>%
  filter(Mois %in% c(1, 2, 3, 11)) %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "2. Semaine Travail (Moyenne)")

profil_vacances <- df_clean_stats %>%
  filter(Mois %in% c(7, 8)) %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "3. Semaine Vacances (Moyenne)")

profil_global <- df_clean_stats %>%
  group_by(Jour_Semaine) %>%
  summarise(Valeur = mean(Total_Reseau), .groups = "drop") %>%
  mutate(Type = "4. Semaine Globale (Moyenne)")

donnees_comparatives <- bind_rows(profil_benchmark, profil_travail, profil_vacances, profil_global)

# On stocke dans 'p4'
p4 <- ggplot(donnees_comparatives, aes(x = Jour_Semaine, y = Valeur, color = Type, group = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparatif des profils hebdomadaires",
    subtitle = "Le Benchmark (Mars 2019) vs Les moyennes historiques (hors 2020)",
    x = "",
    y = "Validations par jour",
    color = "Profil"
  ) +
  scale_color_manual(values = c(
    "1. Benchmark (Mars 2019)" = "black",
    "2. Semaine Travail (Moyenne)" = "#D9303E",
    "3. Semaine Vacances (Moyenne)" = "#2ECC71",
    "4. Semaine Globale (Moyenne)" = "steelblue"
  )) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
  theme(legend.position = "bottom")

# Affichage et Sauvegarde
print(p4)
ggsave("img/4_comparatif_benchmark.png", plot = p4, width = 10, height = 7, dpi = 300)