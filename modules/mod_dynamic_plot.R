

# UI function 
mod_dynamic_plot_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("column_select_x")),
      uiOutput(ns("column_select_y"))
    ),
    mainPanel(
      plotlyOutput(ns("distPlot"))
    )
  )
}

# Server function
mod_dynamic_plot_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(shared_data())
      shared_data()
    })
    
    observe({
      df <- data()
      req(df) 
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      output$column_select_x <- renderUI({
        selectInput(session$ns("xcol"), "Select X Column", choices = numeric_cols)
      })
      
      output$column_select_y <- renderUI({
        selectInput(session$ns("ycol"), "Select Y Column", choices = numeric_cols)
      })
    })
    
    output$distPlot <- renderPlotly({
      req(input$xcol, input$ycol)
      
      df <- data()
      
      plot_ly(
        data = df,
        x = ~ .data[[input$xcol]],
        y = ~ .data[[input$ycol]],
        type = 'scatter',
        mode = 'markers'
      ) %>%
        layout(
          xaxis = list(title = input$xcol),
          yaxis = list(title = input$ycol)
        )
    })
  })
}