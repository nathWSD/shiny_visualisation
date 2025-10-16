library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(plotly)
library(readr)
library(bslib) 
library(shinyjs)


source("modules/mod_dynamic_plot.R")
source("modules/mod_prediction_panel.R")

shiny::addResourcePath(prefix = 'images', directoryPath = 'images')


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("Modular App",
             selected = "Dynamic Plot", 
             tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
             tabPanel("Prediction Panel", mod_prediction_panel_ui("predict"))
  )
)


server <- function(input, output, session) {
  
  data_file_path <- "car_sales_data.csv"
  
  if (!file.exists(data_file_path)) {
    stop(paste("Error: The data file was not found.",
               "Please make sure the file named", data_file_path, 
               "is in the same directory as the app.R file."))
  }
  
  shared_data <- reactiveVal(read_csv(data_file_path))
  
  
  mod_dynamic_plot_server("plot", shared_data)
  mod_prediction_panel_server("predict", shared_data)
}


shinyApp(ui, server)