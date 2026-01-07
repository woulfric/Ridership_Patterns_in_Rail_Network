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