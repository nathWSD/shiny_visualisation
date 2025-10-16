library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)
library(plotly)
library(readr)
library(bslib) 
library(shinyjs)


# Source modules
source("modules/mod_dynamic_plot.R")
source("modules/mod_prediction_panel.R")

shiny::addResourcePath(prefix = 'images', directoryPath = 'images')


# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("Modular App",
             tabPanel("Upload Data",
                      sidebarPanel(
                        fileInput("file", "Upload CSV File", accept = ".csv")
                      ),
                      mainPanel("Upload your dataset to enable other tabs.")
             ),
             tabPanel("Dynamic Plot", mod_dynamic_plot_ui("plot")),
             tabPanel("Prediction Panel", mod_prediction_panel_ui("predict"))
  )
)


# Server
server <- function(input, output, session) {
  shared_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    shared_data(read_csv(input$file$datapath))
  })
  
  mod_dynamic_plot_server("plot", shared_data)
  mod_prediction_panel_server("predict", shared_data)
}


shinyApp(ui, server)
