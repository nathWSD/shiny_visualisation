# --- Helper: Tooltip ---
labelWithTooltip <- function(labelText, tooltipText) {
  tags$div(
    class = "d-flex align-items-center gap-1 mb-1",
    tags$span(labelText, style = "font-weight: 500;"),
    tooltip(
      trigger = tryCatch(bsicons::bs_icon("info-circle-fill", class = "text-primary", size = "0.9rem"), error = function(e) icon("info-circle")),
      tooltipText
    )
  )
}

# --- UI MODULE ---
mod_prediction_panel_ui <- function(id) {
  ns <- NS(id)
  
  # --- Config Loading ---
  config_path <- "ui_config.json"
  if (file.exists(config_path)) {
    ui_config <- fromJSON(config_path)
    color_css_map <- ui_config$color_map
  } else {
    ui_config <- list(manufacturer_models = list("ERROR" = c("ui_config.json not found")), 
                      body_type = "SUV", transmission = "Automatic", drivetrain = "AWD", 
                      exterior_colour = "black", interior_colour = "black", fuel_type = "gasoline", 
                      engine_type = "Inline")
    color_css_map <- list("black" = "#000000")
  }
  
  prepare_color_data <- function(color_names) {
    codes <- sapply(color_names, function(name) {
      code <- color_css_map[[name]]
      if (is.null(code)) "#777777" else code
    }, USE.NAMES = FALSE)
    mapply(function(name, code) {
      list(value = name, label = name, color_code = code)
    }, color_names, codes, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  
  exterior_color_data <- prepare_color_data(ui_config$exterior_colour)
  interior_color_data <- prepare_color_data(ui_config$interior_colour)
  
  render_js <- I("{
      item: function(item, escape) { return '<div><span class=\"color-swatch\" style=\"background-color: ' + item.color_code + ';\"></span>' + escape(item.label) + '</div>'; },
      option: function(item, escape) { return '<div><span class=\"color-swatch\" style=\"background-color: ' + item.color_code + ';\"></span>' + escape(item.label) + '</div>'; }
    }")
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        .fixed-height-card {
            height: 100% !important; 
            display: flex;
            flex-direction: column;
            overflow: hidden; /* Prevent the card itself from scrolling */
        }

        .scrollable-card-body { 
            flex: 1 1 auto;
            overflow-y: auto; 
            min-height: 0; /* Crucial CSS fix for nested flex containers */
            padding-right: 5px; /* Prevent scrollbar from hiding content */
        }
        
        .card-header { font-size: 1.6rem !important; font-weight: 700 !important; padding-top: 15px !important; padding-bottom: 15px !important; flex: 0 0 auto; }
        .card-footer { flex: 0 0 auto; } /* Ensure footer doesn't shrink */
        
        .irs-grid-text { visibility: hidden !important; }
        .irs-grid-text:nth-of-type(3n+1) { visibility: visible !important; }
        .irs-grid-text:last-of-type { visibility: visible !important; }
        .irs--shiny .irs-bar { border-top: 1px solid #0d6efd; border-bottom: 1px solid #0d6efd; background: #0d6efd; }
        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #0d6efd; }

        .color-swatch { display: inline-block; width: 15px; height: 15px; border-radius: 50%; margin-right: 8px; vertical-align: middle; border: 1px solid #ddd; }
      ")))
    ),
    
    # --- MAIN LAYOUT ---
    page_fillable(
      padding = 20,
      gap = 20,
      
      layout_columns(
        col_widths = c(6, 6),
        height = "85vh", 
        fill = TRUE,
        
        # --- LEFT CARD: INPUTS ---
        card(
          class = "fixed-height-card",
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Vehicle Configuration"),
          
          card_body(
            class = "scrollable-card-body",
            accordion(
              id = ns("collapse_inputs"),
              multiple = TRUE, 
              open = "General Information",
              
              accordion_panel(
                "General Information",
                icon = icon("car"),
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput(ns("manufacturer"), labelWithTooltip("Manufacturer", "Select brand."), choices = names(ui_config$manufacturer_models), width = "100%"),
                  uiOutput(ns("model_ui"))
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  sliderInput(ns("year_of_manufacture"), labelWithTooltip("Year", "Year of production."), min = 1940, max = 2025, value = 2018, step = 1, sep = "", width = "100%"),
                  selectInput(ns("body_type"), labelWithTooltip("Body Type", "Chassis style."), choices = ui_config$body_type, width = "100%")
                )
              ),
              
              accordion_panel(
                "Drive & Engine",
                icon = icon("cogs"),
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput(ns("engine_type"), labelWithTooltip("Engine Type", "Configuration."), choices = ui_config$engine_type, width = "100%"),
                  sliderInput(ns("engine_displacement_L"), labelWithTooltip("Displacement (L)", "Size in Liters."), min = 0.6, max = 8.0, value = 2.0, step = 0.1, width = "100%")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  sliderInput(ns("engine_cylinders"), labelWithTooltip("Cylinders", "Count."), min = 0, max = 16, value = 4, step = 1, width = "100%"),
                  selectInput(ns("drivetrain"), labelWithTooltip("Drivetrain", "Wheel drive system."), choices = ui_config$drivetrain, width = "100%")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  selectInput(ns("transmission"), labelWithTooltip("Transmission", "Gearbox type."), choices = ui_config$transmission, width = "100%"),
                  selectInput(ns("fuel_type"), labelWithTooltip("Fuel Type", "Fuel source."), choices = ui_config$fuel_type, width = "100%")
                )
              ),
              
              accordion_panel(
                "Consumption & Performance",
                icon = icon("tachometer-alt"),
                layout_columns(
                  col_widths = c(6, 6),
                  sliderInput(ns("city_consumption"), labelWithTooltip("City (L/100km)", "Urban consumption."), min = 2, max = 25, value = 11.0, step = 0.1, width = "100%"),
                  sliderInput(ns("highway_consumption"), labelWithTooltip("Highway (L/100km)", "Extra-urban consumption."), min = 0, max = 20, value = 8.5, step = 0.1, width = "100%")
                ),
                sliderInput(ns("mileage"), labelWithTooltip("Mileage (km)", "Total distance."), min = 0, max = 800000, value = 80000, step = 500, width = "100%")
              ),
              
              accordion_panel(
                "Equipment & Design",
                icon = icon("paint-brush"),
                layout_columns(
                  col_widths = c(6, 6),
                  selectizeInput(ns("exterior_colour"), labelWithTooltip("Exterior Colour", "Paint."), choices = ui_config$exterior_colour, width = "100%", options = list(options = exterior_color_data, valueField = 'value', labelField = 'label', searchField = 'label', render = render_js)),
                  selectizeInput(ns("interior_colour"), labelWithTooltip("Interior Colour", "Upholstery."), choices = ui_config$interior_colour, width = "100%", options = list(options = interior_color_data, valueField = 'value', labelField = 'label', searchField = 'label', render = render_js))
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  sliderInput(ns("passengers"), labelWithTooltip("Passengers", "Seats."), min = 2, max = 14, value = 5, step = 1, width = "100%"),
                  sliderInput(ns("doors"), labelWithTooltip("Doors", "Door count."), min = 2, max = 5, value = 4, step = 1, width = "100%")
                )
              )
            )
          ),
          
          card_footer(
            class = "bg-light",
            tags$div(
              class = "d-grid gap-2",
              fileInput(ns("car_images"), label = NULL, buttonLabel = "Upload Photos...", placeholder = "Required for AI Analysis", multiple = TRUE, accept = c("image/jpeg", "image/png", "image/jpg"), width = "100%"),
              actionButton(ns("submitbutton"), "Predict Price", icon = icon("calculator"), class = "btn-primary btn-lg")
            )
          )
        ),
        
        # --- RIGHT CARD: RESULTS ---
        card(
          class = "fixed-height-card",
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Market Valuation"),
          
          card_body(
            class = "scrollable-card-body",
            uiOutput(ns("image_analysis_output")),
            uiOutput(ns("contents")),
            hr(),
            div(style = "height: 400px;", plotlyOutput(ns("importance_plot"), height = "100%"))
          )
        )
      )
    )
  )
}

mod_prediction_panel_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
        
    shinyjs::disable("submitbutton")
    
    observeEvent(input$car_images, {
      if (!is.null(input$car_images) && nrow(input$car_images) > 0) {
        shinyjs::enable("submitbutton")
      } else {
        shinyjs::disable("submitbutton")
      }
    }, ignoreNULL = FALSE)
    
    config_data <- reactiveVal(NULL)
    observe({
      config_path <- "ui_config.json"
      if (file.exists(config_path)) {
        config_data(fromJSON(config_path))
      } else {
        config_data(list(manufacturer_models = list("ERROR" = c("ui_config.json not found"))))
      }
    })
    
    output$model_ui <- renderUI({
      req(input$manufacturer, config_data())
      models <- config_data()$manufacturer_models[[input$manufacturer]]
      if (is.null(models)) return(NULL)
      selectInput(ns("model"), label = labelWithTooltip("Model:", "Select the car model."), choices = models, width = "100%")
    })
    
    trained_model_bundle <- reactiveVal(NULL)
    importance_plot_obj <- reactiveVal(NULL)
    
    # --- Main Prediction Event ---
    observeEvent(input$submitbutton, {
      req(input$model, input$car_images, cancelOutput = TRUE)
      
      withProgress(message = 'Processing Request', style = "old", value = 0, {
        
        model_paths <- list(
          lower = "models/ranger_lower.rds", 
          median = "models/ranger_median.rds", 
          upper = "models/ranger_upper.rds", 
          preproc = "models/ranger_preproc_info.rds",
          image_classifier = "models/car_damage_classifier.h5"
        )
        
        if (is.null(trained_model_bundle())) {
          if (!all(sapply(model_paths, file.exists))) {
            stop("FATAL ERROR: Model files not found. Please run the training scripts first.")
          }
          setProgress(value = 0.1, detail = "Loading models...")
          bundle <- list(
            models = lapply(model_paths[c("lower", "median", "upper")], readRDS), 
            preproc_info = readRDS(model_paths$preproc),
            image_model = load_model_hdf5(model_paths$image_classifier)
          )
          trained_model_bundle(bundle)
        }
        
        current_bundle <- trained_model_bundle()
        
        # --- 2. IMAGE ANALYSIS ---
        setProgress(value = 0.3, detail = "Analyzing car images...")
        
        state_description_map <- c(
          `1` = "Very Good", `2` = "Minor Damage",
          `3` = "Moderate Damage", `4` = "Severe Damage"
        )
        
        image_paths <- input$car_images$datapath
        
        predicted_classes_numeric <- sapply(image_paths, function(path) {
          img <- image_load(path, target_size = c(224, 224))
          img_array <- image_to_array(img)
          img_array <- array_reshape(img_array, c(1, 224, 224, 3))
          preds <- current_bundle$image_model %>% predict(img_array)
          predicted_class_index <- which.max(preds) 
          return(predicted_class_index)
        })
        
        avg_state <- mean(predicted_classes_numeric)
        final_car_state <- floor(avg_state + 0.5)
        
        descriptive_name <- state_description_map[as.character(final_car_state)]
        
        image_feedback_message <- paste0(
          "Analyzed ", length(image_paths), " image(s). ",
          "Determined average condition: '", descriptive_name, "'."
        )
        
      
        output$image_analysis_output <- renderUI({
          tags$div(
            class = "alert alert-info",
            style = "margin-top: 10px;",
            tags$h5(style = "font-weight: bold; margin-bottom: 5px;", icon("camera"), " Image Analysis Result"),
            tags$p(style = "margin-bottom: 0;", image_feedback_message)
          )
        })
        
        # ---  TABULAR DATA PREPARATION ---
        setProgress(value = 0.5, detail = "Preparing final data...")
        
        newdata <- data.frame(
          year_of_manufacture = as.integer(input$year_of_manufacture),
          manufacturer = input$manufacturer,
          model = input$model,
          mileage = as.numeric(input$mileage), 
          body_type = input$body_type,
          transmission = input$transmission, 
          drivetrain = input$drivetrain,
          exterior_colour = input$exterior_colour,
          interior_colour = input$interior_colour, 
          passengers = as.integer(input$passengers),
          doors = as.integer(input$doors),
          fuel_type = input$fuel_type,
          city_consumption = as.numeric(input$city_consumption), 
          highway_consumption = as.numeric(input$highway_consumption),
          engine_displacement_L = as.numeric(input$engine_displacement_L), 
          engine_cylinders = as.integer(input$engine_cylinders),
          engine_type = input$engine_type,
          car_state = final_car_state 
        )
        
        preproc_info <- current_bundle$preproc_info
        for (col in names(preproc_info$all_levels)) {
          if (col %in% names(newdata)) {
            newdata[[col]] <- factor(newdata[[col]], levels = preproc_info$all_levels[[col]])
          }
        }
        
        # ---  RANGER PRICE PREDICTION ---
        setProgress(value = 0.9, detail = "Generating price prediction...")
        
        if (!inherits(current_bundle$models$median, "ranger")) {
          showNotification("Error: Loaded model is not a ranger object.", type = "error")
          return(NULL)
        }
        
        predictions <- list(
          lower = predict(current_bundle$models$lower, data = newdata, type = "quantiles", quantiles = 0.05)$predictions[,1],
          median = predict(current_bundle$models$median, data = newdata, type = "quantiles", quantiles = 0.50)$predictions[,1],
          upper = predict(current_bundle$models$upper, data = newdata, type = "quantiles", quantiles = 0.95)$predictions[,1]
        )
        
        # ---  RENDER OUTPUTS ---
        imp_raw <- ranger::importance(current_bundle$models$median)
        
        if (length(imp_raw) > 0) {
          imp_df <- data.frame(
            Feature = names(imp_raw),
            Importance = imp_raw,
            row.names = NULL
          ) %>%
            arrange(Importance)
          
          p <- plot_ly(
            data = imp_df, 
            x = ~Importance, 
            y = ~factor(Feature, levels = Feature), 
            type = 'bar', 
            orientation = 'h'
          ) %>%
            layout(
              title = "", 
              yaxis = list(title = ""), 
              xaxis = list(title = "Feature Importance (Impurity)"),
              paper_bgcolor = 'rgba(0,0,0,0)', 
              plot_bgcolor = 'rgba(0,0,0,0)'
            )
          importance_plot_obj(p)
        } else {
          importance_plot_obj(NULL)
        }
        
        output$contents <- renderUI({
          
          pred_median <- round(predictions$median)
          pred_lower <- round(predictions$lower)
          pred_upper <- round(predictions$upper)
          
          format_euro <- function(amount) {
            paste0(format(amount, nsmall = 0, big.mark = ","), " €")
          }
          
          tags$div(
            tags$style(HTML("
              .pred-table { width: 100%; border-collapse: collapse; margin-top: 15px; }
              .pred-table td { padding: 8px; border: 1px solid #ddd; text-align: right; }
              .pred-table td:first-child { text-align: left; font-weight: bold; }
            ")),
            tags$table(class = "pred-table",
                       tags$tr(
                         tags$td("Predicted Price (Median)"),
                         tags$td(format_euro(pred_median))
                       ),
                       tags$tr(
                         tags$td("90% Confidence Lower Bound"),
                         tags$td(format_euro(pred_lower))
                       ),
                       tags$tr(
                         tags$td("90% Confidence Upper Bound"),
                         tags$td(format_euro(pred_upper))
                       ),
                       tags$tr(
                         tags$td("Uncertainty Range"),
                         tags$td(paste0("± ", format_euro((pred_upper - pred_lower) / 2)))
                       )
            ),
            tags$br(),
            tags$p(style = "text-align: center;",
                   "Based on the provided specifications, the model predicts the car's price to be around ",
                   tags$b(format_euro(pred_median)),
                   ", with a 90% confidence that the true price falls between ",
                   tags$b(format_euro(pred_lower)), " and ", tags$b(format_euro(pred_upper)), "."
            )
          )
        })
      })
    })
    
    output$importance_plot <- renderPlotly({
      req(importance_plot_obj())
      importance_plot_obj()
    })
  })
}
