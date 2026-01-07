library(shiny)
library(tidyverse)
library(leaflet)
library(scales)

# --- 1. CHARGEMENT ET PRÉPARATION ---
df <- readRDS("data_clean.rds")

# Préparation Données Carte
df_map <- df %>%
  mutate(Code_Postal = as.character(VILLE)) %>%
  mutate(DEPT = substr(Code_Postal, 1, 2)) %>%
  group_by(NOM_GARE, ID_REFA_LDA) %>%
  summarise(
    Trafic_Moyen = mean(NB_VALD, na.rm = TRUE),
    LON = first(LON),
    LAT = first(LAT),
    Code_Postal = first(Code_Postal),
    DEPT = first(DEPT),
    .groups = "drop"
  ) %>%
  filter(!is.na(LON) & !is.na(LAT))

liste_depts <- sort(unique(df_map$DEPT))

# --- 2. UI (Interface) ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("🚇 Dashboard Trafic Île-de-France"),
  
  tabsetPanel(
    
    # === ONGLET 1 : CARTE ===
    tabPanel("Exploration Géographique", icon = icon("map"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("📍 Filtrer par Zone"),
                 selectizeInput("choix_dept", "Choisir Départements :", choices = liste_depts, multiple = TRUE, options = list(placeholder = 'Ex: 75, 92...')),
                 actionButton("btn_reset", "Voir tout", icon = icon("globe"), class="btn-sm btn-info"),
                 hr(),
                 wellPanel(h5("Aide Codes :"), tags$ul(tags$li("75 : Paris"), tags$li("92 : Hauts-de-Seine"), tags$li("77, 78... : Grande Couronne")))
               ),
               mainPanel(
                 leafletOutput("map_res", height = "500px"),
                 br(),
                 wellPanel(h4(textOutput("titre_graphique")), plotOutput("plot_trend", height = "250px"))
               )
             )
    ),
    
    # === ONGLET 2 : COMPARATEUR (Avec Auto-Correcteur) ===
    tabPanel("Comparaison de Périodes", icon = icon("chart-bar"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Sélection des Semaines"),
                 p(style="font-size:0.9em; color:grey", "Le système sélectionne automatiquement des semaines complètes (Lun-Dim)."),
                 
                 dateRangeInput("date_ref", "1. Période Référence", start = "2019-03-04", end = "2019-03-10", weekstart = 1),
                 dateRangeInput("date_comp", "2. Période à Comparer", start = "2023-12-25", end = "2023-12-31", weekstart = 1),
                 
                 actionButton("btn_compare", "Comparer les périodes", class = "btn-primary btn-block")
               ),
               mainPanel(
                 plotOutput("plot_compare", height = "400px"),
                 tableOutput("table_compare")
               )
             )
    )
  )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  # === NOUVEAU : AUTO-CORRECTEUR DE DATES ===
  # Fonction qui vérifie et corrige les dates
  corriger_dates <- function(input_id, date_range) {
    debut <- date_range[1]
    fin <- date_range[2]
    
    # On calcule le Lundi précédent et le Dimanche suivant
    # week_start = 1 signifie Lundi
    debut_clean <- floor_date(debut, "week", week_start = 1)
    fin_clean <- ceiling_date(fin, "week", week_start = 1) - days(1) 
    # (ceiling donne le lundi d'après, donc on enlève 1 jour pour avoir dimanche)
    
    # Si les dates ne sont pas déjà "propres", on force la mise à jour
    if (debut != debut_clean || fin != fin_clean) {
      updateDateRangeInput(session, input_id, start = debut_clean, end = fin_clean)
      showNotification("Dates ajustées pour couvrir des semaines complètes (Lun-Dim).", type = "message", duration = 3)
    }
  }

  # On surveille les changements de dates et on applique la correction
  observeEvent(input$date_ref, { corriger_dates("date_ref", input$date_ref) })
  observeEvent(input$date_comp, { corriger_dates("date_comp", input$date_comp) })

  
  # === RESTE DU CODE (CARTE & GRAPHIQUES) ===
  
  data_map_filtered <- reactive({
    if (is.null(input$choix_dept)) return(df_map) else return(df_map %>% filter(DEPT %in% input$choix_dept))
  })
  
  output$map_res <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(2.35, 48.85, 10)
  })
  
  observe({
    donnees <- data_map_filtered()
    req(nrow(donnees) > 0)
    leafletProxy("map_res", data = donnees) %>%
      clearMarkers() %>%
      addCircleMarkers(lng=~LON, lat=~LAT, layerId=~NOM_GARE, radius=~(sqrt(Trafic_Moyen)/15)+4, 
                       color="#18458A", fillOpacity=0.6, stroke=TRUE, weight=1, popup=~paste0("<b>",NOM_GARE,"</b><br>CP: ",Code_Postal))
    if (!is.null(input$choix_dept)) leafletProxy("map_res", data=donnees) %>% fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
  })
  
  observeEvent(input$btn_reset, { updateSelectizeInput(session, "choix_dept", selected = "") })
  
  observeEvent(input$map_res_marker_click, {
    click <- input$map_res_marker_click
    if(!is.null(click)) {
      output$titre_graphique <- renderText({ paste("Station :", click$id) })
      output$plot_trend <- renderPlot({
        df %>% filter(NOM_GARE == click$id) %>% group_by(JOUR) %>% summarise(Total=sum(NB_VALD, na.rm=TRUE)) %>%
          ggplot(aes(x=JOUR, y=Total)) + geom_line(color="steelblue", alpha=0.5) + geom_smooth(method="loess", color="#D9303E", se=FALSE) + theme_minimal() + labs(x="", y="Validations")
      })
    }
  })
  
  output$plot_trend <- renderPlot({
    req(is.null(input$map_res_marker_click))
    ggplot() + theme_void() + geom_text(aes(0,0,label="Sélectionnez une gare sur la carte"))
  })
  
  comparaison_data <- eventReactive(input$btn_compare, {
    ref <- df %>% filter(JOUR >= input$date_ref[1] & JOUR <= input$date_ref[2]) %>%
      mutate(Jour_Semaine = wday(JOUR, label=TRUE, week_start=1)) %>% group_by(Jour_Semaine) %>% 
      summarise(Moyenne = mean(tapply(NB_VALD, JOUR, sum), na.rm=TRUE)) %>% mutate(Periode = "Référence")
    comp <- df %>% filter(JOUR >= input$date_comp[1] & JOUR <= input$date_comp[2]) %>%
      mutate(Jour_Semaine = wday(JOUR, label=TRUE, week_start=1)) %>% group_by(Jour_Semaine) %>% 
      summarise(Moyenne = mean(tapply(NB_VALD, JOUR, sum), na.rm=TRUE)) %>% mutate(Periode = "Comparaison")
    bind_rows(ref, comp)
  })
  
  output$plot_compare <- renderPlot({
    req(comparaison_data())
    ggplot(comparaison_data(), aes(x=Jour_Semaine, y=Moyenne, fill=Periode)) +
      geom_col(position="dodge") + theme_minimal() + scale_fill_manual(values=c("#18458A", "#D9303E")) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M"))
  })
  
  output$table_compare <- renderTable({
    req(comparaison_data())
    comparaison_data() %>% pivot_wider(names_from=Periode, values_from=Moyenne) %>%
      mutate(Evol = paste0(round(((Comparaison-Référence)/Référence)*100, 1), "%"))
  })
}

shinyApp(ui, server)