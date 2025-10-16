

labelWithTooltip <- function(labelText, tooltipText) {
  tags$label(
    labelText,
    tags$span(
      class = "tooltip-container",
      shiny::icon("info-circle", style = "margin-left: 5px; color: #007bff; cursor: help;"),
      tags$span(class = "tooltip-text", tooltipText)
    )
  )
}


mod_dynamic_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* --- General Layout & Panel Styling (Unchanged) --- */
        #", ns("plot_container"), " {
          height: calc(100vh - 80px); display: flex; flex-direction: row;
          align-items: stretch; padding: 20px; gap: 20px;
        }
        #", ns("plot_sidebar"), ", #", ns("plot_main_panel"), " {
          background-color: rgba(255, 255, 255, 0.9); border-radius: 10px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15); padding: 25px; overflow-y: auto;
        }
        #", ns("generate_plot"), " { margin-top: 20px; width: 100%; }

        /* --- 2. ADD THE CSS FOR THE CUSTOM TOOLTIPS --- */
        .tooltip-container {
          position: relative;
          display: inline-block;
        }
        .tooltip-text {
          visibility: hidden; width: 180px; background-color: #333; color: #fff;
          text-align: center; border-radius: 6px; padding: 5px 10px;
          position: absolute; z-index: 10; bottom: 125%; left: 50%;
          margin-left: -90px; opacity: 0; transition: opacity 0.3s; font-weight: normal;
        }
        .tooltip-container:hover .tooltip-text {
          visibility: visible;
          opacity: 1;
        }
      ")))
    ),
    
    div(
      id = ns("plot_container"),
      
      column(
        width = 4,
        id = ns("plot_sidebar"),
        
        selectInput(ns("plot_type"), 
                    label = labelWithTooltip("Select Plot Type", "Choose the type of chart to display."),
                    choices = c("Scatter Plot", "Line Plot", "Bar Chart"),
                    width = "100%"), 
        
        selectInput(ns("x_col"), 
                    label = labelWithTooltip("Select X Column (Category)", "For bar charts, this is the categorical axis. For other plots, this is the numeric x-axis."), 
                    choices = NULL,
                    width = "100%"), 
        
        selectInput(ns("y_col"), 
                    label = labelWithTooltip("Select Y Column (Value)", "This is the numeric value axis for all plot types."), 
                    choices = NULL,
                    width = "100%"), 
        
        actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary")
      ),
      
      column(
        width = 8,
        id = ns("plot_main_panel"),
        plotlyOutput(ns("dynamic_plot"), height = "100%")
      )
    )
  )
}


mod_dynamic_plot_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$plot_type, {
      df <- shared_data()
      req(df)
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      categorical_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      
      if (input$plot_type == "Bar Chart") {
        updateSelectInput(session, "x_col", choices = categorical_cols)
        updateSelectInput(session, "y_col", choices = numeric_cols)
      } else {
        updateSelectInput(session, "x_col", choices = numeric_cols)
        updateSelectInput(session, "y_col", choices = numeric_cols)
      }
    })
    
    plot_object <- reactiveVal(NULL)
    
    observeEvent(input$generate_plot, {
      df <- shared_data()
      req(df, input$x_col, input$y_col, input$plot_type)
      
      p <- switch(
        input$plot_type,
        
        "Scatter Plot" = {
          plot_ly(df, x = ~get(input$x_col), y = ~get(input$y_col),
                  type = 'scatter', mode = 'markers',
                  marker = list(color = '#007bff', size = 8))
        },
        
        "Line Plot" = {
          df_sorted <- df %>% arrange(!!sym(input$x_col))
          plot_ly(df_sorted, x = ~get(input$x_col), y = ~get(input$y_col),
                  type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#007bff'),
                  marker = list(color = '#007bff', size = 8))
        },
        
        "Bar Chart" = {
          df_agg <- df %>%
            group_by(!!sym(input$x_col)) %>%
            summarise(agg_y = mean(!!sym(input$y_col), na.rm = TRUE), .groups = 'drop') %>%
            rename(y_val = agg_y, x_cat = !!sym(input$x_col))
          
          plot_ly(df_agg, x = ~x_cat, y = ~y_val,
                  type = 'bar', marker = list(color = '#007bff'))
        }
      )
      
      p <- p %>% layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        xaxis = list(title = input$x_col, color = '#333333', gridcolor = 'rgba(128, 128, 128, 0.5)'),
        yaxis = list(title = input$y_col, color = '#333333', gridcolor = 'rgba(128, 128, 128, 0.5)'),
        font = list(color = '#333333')
      )
      
      plot_object(p)
    })
    
    output$dynamic_plot <- renderPlotly({
      if (is.null(plot_object())) {
        return(
          plot_ly() %>%
            layout(
              paper_bgcolor = 'rgba(0,0,0,0)',
              plot_bgcolor = 'rgba(0,0,0,0)',
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              annotations = list(
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                text = "Please select your options and click 'Generate Plot'",
                showarrow = FALSE, font = list(size = 16, color = '#555555')
              )
            )
        )
      }
      plot_object()
    })
    
  })
}