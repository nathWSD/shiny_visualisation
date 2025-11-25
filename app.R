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
library(keras)
library(tensorflow)
library(Matrix)
library(caret)
library(jsonlite)
library(rstudioapi)
library(shinyBS)
library(thematic)
library(ranger)
library(bsicons)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# --- Source Modules ---

source("modules/mod_dynamic_plot.R")
source("modules/mod_prediction_panel.R")
source("modules/mod_theme_changer.R") 



# --- UI ---
ui <- navbarPage(
  title = "Auto Market",
  
  
  theme = bslib::bs_theme(bootswatch = "cerulean", version = 5),
  
  selected = "Prediction Panel", 
  
  tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
  tabPanel("Prediction Panel", mod_prediction_panel_ui("predict")),
  tabPanel("Settings", mod_theme_changer_ui("theme_changer")) 
)

# --- SERVER ---
server <- function(input, output, session) {
  
  
  thematic::thematic_shiny()
  
  data_file_path <- "detailed_car_sales_data_all.csv"
  
  if (!file.exists(data_file_path)) {
    stop(paste("Error: The data file was not found.",
               "Please make sure the file named", data_file_path, 
               "is in the same directory as the app.R file."))
  }
  
  shared_data <- reactiveVal(read_csv(data_file_path))
  
  mod_dynamic_plot_server("plot", shared_data)
  mod_prediction_panel_server("predict", shared_data)
  
  # Call the theme changer module
  mod_theme_changer_server("theme_changer") 
}

shinyApp(ui, server)