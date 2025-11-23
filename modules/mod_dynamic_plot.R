# modules/mod_dynamic_plot.R

labelWithTooltip <- function(labelText, tooltipText) {
  tags$label(
    labelText,
    tags$span(
      class = "tooltip-container",
      shiny::icon("info-circle", style = "margin-left: 5px; color: var(--bs-primary); cursor: help;"),
      tags$span(class = "tooltip-text", tooltipText)
    )
  )
}

mod_dynamic_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        #", ns("plot_container"), " {
          height: calc(100vh - 80px); 
          display: flex; 
          flex-direction: row;
          align-items: stretch; 
          padding: 20px; 
          gap: 20px;
        }
        /* Use Bootstrap CSS variables (var(--bs-...)) 
           This allows the box to automatically turn black/grey in Dark Mode 
        */
        #", ns("plot_sidebar"), ", #", ns("plot_main_panel"), " {
          background-color: var(--bs-card-bg, #ffffff); 
          color: var(--bs-body-color, #333333);
          border: 1px solid var(--bs-border-color, #dee2e6);
          border-radius: 10px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15); 
          padding: 25px; 
          overflow-y: auto;
        }
        #", ns("generate_plot"), " { margin-top: 20px; width: 100%; }

        .tooltip-container {
          position: relative;
          display: inline-block;
        }
        .tooltip-text {
          visibility: hidden; width: 180px; 
          background-color: var(--bs-body-color, #333); /* Invert bg relative to theme */
          color: var(--bs-body-bg, #fff);              /* Invert text relative to theme */
          text-align: center; border-radius: 6px; padding: 5px 10px;
          position: absolute; z-index: 10; bottom: 125%; left: 50%;
          margin-left: -90px; opacity: 0; transition: opacity 0.3s; font-weight: normal;
          border: 1px solid var(--bs-border-color);
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
                    choices = c("Scatter Plot", "Bar Chart"),
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
    
    # 1. Update Dropdowns based on data
    observe({
      df <- shared_data()
      req(df, input$plot_type)
      
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
    
    # 2. Reactive trigger for plot generation
    # We separate the "calculation" from the "rendering" to handle themes better
    plot_geometry <- reactive({
      input$generate_plot # Trigger on button click
      
      isolate({
        req(shared_data(), input$x_col, input$y_col, input$plot_type)
        
        df <- shared_data()
        type <- input$plot_type
        x_c <- input$x_col
        y_c <- input$y_col
        
        list(df = df, type = type, x = x_c, y = y_c)
      })
    })
    
    # 3. Render Plotly
    # This runs when data changes OR when the THEME changes
    output$dynamic_plot <- renderPlotly({
      
      # A. Theme Extraction Logic
      # We ask bslib for the current colors so we can paint the plot correctly
      theme <- bslib::bs_current_theme()
      
      # Defaults (Light mode)
      fg_col <- "#333333"
      bg_col <- "rgba(0,0,0,0)" # Transparent
      primary_col <- "#007bff"
      grid_col <- "rgba(128, 128, 128, 0.2)"
      
      # Overwrite defaults if a bslib theme is active
      if (bslib::is_bs_theme(theme)) {
        vars <- bslib::bs_get_variables(theme, c("body-color", "card-bg", "primary", "border-color"))
        fg_col <- vars[["body-color"]]
        # We use transparent for plot background so it blends with the card
        bg_col <- "rgba(0,0,0,0)" 
        primary_col <- vars[["primary"]]
        grid_col <- vars[["border-color"]] # Use the theme's border color for grid lines
      }
      
      # B. Get Data (from the reactive above)
      geom <- plot_geometry()
      
      # Handle "Not generated yet" state
      if (is.null(geom)) {
        return(
          plot_ly() %>%
            layout(
              paper_bgcolor = bg_col,
              plot_bgcolor = bg_col,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              annotations = list(
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                text = "Please select your options and click 'Generate Plot'",
                showarrow = FALSE, 
                font = list(size = 16, color = fg_col) # Dynamic Text Color
              )
            )
        )
      }
      
      # C. Build the Plot
      df <- geom$df
      p <- NULL
      
      if (geom$type == "Scatter Plot") {
        p <- plot_ly(df, x = ~get(geom$x), y = ~get(geom$y),
                     type = 'scatter', mode = 'markers',
                     # Use the Dynamic Primary Color
                     marker = list(color = primary_col, size = 8))
      } else if (geom$type == "Bar Chart") {
        df_agg <- df %>%
          group_by(!!sym(geom$x)) %>%
          summarise(agg_y = mean(!!sym(geom$y), na.rm = TRUE), .groups = 'drop') %>%
          rename(y_val = agg_y, x_cat = !!sym(geom$x))
        
        p <- plot_ly(df_agg, x = ~x_cat, y = ~y_val,
                     type = 'bar', 
                     # Use the Dynamic Primary Color
                     marker = list(color = primary_col))
      }
      
      # D. Apply Theme Layout
      p %>% layout(
        paper_bgcolor = bg_col,
        plot_bgcolor = bg_col,
        # Dynamic Axis Colors
        xaxis = list(
          title = geom$x, 
          color = fg_col, 
          gridcolor = grid_col,
          zerolinecolor = grid_col
        ),
        yaxis = list(
          title = geom$y, 
          color = fg_col, 
          gridcolor = grid_col,
          zerolinecolor = grid_col
        ),
        font = list(color = fg_col) # Global font color
      )
    })
    
  })
}