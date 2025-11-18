library(shiny)
library(data.table)
library(RCurl)
library(randomForest)
library(plotly)
library(readr)
library(bslib) 
library(shinyjs)
library(rlang)
library(dplyr)
library(xgboost)
library(Matrix)
library(caret)
library(jsonlite)

# --- Source Modules ---
source("modules/mod_dynamic_plot.R")
source("modules/mod_prediction_panel.R")

shiny::addResourcePath(prefix = 'detailed_images', directoryPath = 'detailed_images')

# --- UI Definition ---
ui <- navbarPage(
  title = "Auto Market",
  
  # Start-Theme (Wichtig: Version 5)
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  # Header mit Dropdown
  header = tagList(
    tags$head(
      tags$style(HTML("
        #theme_selector_container {
          position: absolute;
          top: 12px;
          right: 15px;
          z-index: 10000;
          width: 180px;
        }
        /* Damit der Inhalt nicht unter der Navbar verschwindet */
        body { padding-top: 0px; }
      "))
    ),
    div(id = "theme_selector_container",
        selectInput("theme_select", NULL, 
                    # HIER IST DIE ÄNDERUNG:
                    # Lädt automatisch alle verfügbaren Bootstrap 5 Themes
                    choices = bslib::bootswatch_themes(version = 5),
                    selected = "cerulean")
    )
  ),
  
  selected = "Prediction Panel", 
  
  tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
  tabPanel("Prediction Panel", mod_prediction_panel_ui("predict"))
)


server <- function(input, output, session) {
  
  # --- Theme Wechsel Logik ---
  observeEvent(input$theme_select, {
    
    # Theme setzen
    session$setCurrentTheme(
      bs_theme(version = 5, bootswatch = input$theme_select)
    )
    
  }, ignoreInit = TRUE)
  
  
  # --- Daten laden ---
  data_file_path <- "detailed_car_sales_data_train.csv"
  shared_data <- reactiveVal()
  
  if (file.exists(data_file_path)) {
    shared_data(read_csv(data_file_path, show_col_types = FALSE))
  } 
  
  observe({
    req(shared_data())
    mod_dynamic_plot_server("plot", shared_data)
    mod_prediction_panel_server("predict", shared_data)
  })
}

shinyApp(ui, server)