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



mod_prediction_panel_ui <- function(id) {
  ns <- NS(id)
  
  config_path <- "ui_config.json"
  if (file.exists(config_path)) {
    ui_config <- fromJSON(config_path)
    color_css_map <- ui_config$color_map
  } else {
    ui_config <- list(manufacturer_models = list("ERROR" = c("ui_config.json not found")), body_type = "SUV", transmission = "Automatic", drivetrain = "AWD", exterior_colour = "black", interior_colour = "black", fuel_type = "gasoline", engine_type = "Inline")
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
  
  render_js <- I(
    "{
      item: function(item, escape) { return '<div><span class=\"color-swatch\" style=\"background-color: ' + item.color_code + ';\"></span>' + escape(item.label) + '</div>'; },
      option: function(item, escape) { return '<div><span class=\"color-swatch\" style=\"background-color: ' + item.color_code + ';\"></span>' + escape(item.label) + '</div>'; }
    }"
  )
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        #", ns("main_container"), " { display: flex; flex-direction: row; height: calc(100vh - 80px); padding: 20px; gap: 20px; }
        #", ns("sidebar"), " { width: 50%; flex: 0 0 50%; background-color: rgba(255, 255, 255, 0.9); border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); padding: 25px; overflow-y: auto; }
        #", ns("main_panel"), " { width: 50%; flex: 1 1 50%; background-color: rgba(255, 255, 255, 0.9); border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); padding: 25px; overflow-y: auto; }
        #", ns("sidebar"), " .form-group { margin-bottom: 20px; }
        .color-swatch { display: inline-block; width: 15px; height: 15px; border-radius: 3px; margin-right: 8px; vertical-align: middle; border: 1px solid #ccc; }
        .tooltip-container { position: relative; display: inline-block; }
        .tooltip-text { visibility: hidden; width: 200px; background-color: #333; color: #fff; text-align: center; border-radius: 6px; padding: 5px 10px; position: absolute; z-index: 10; bottom: 110%; left: 50%; margin-left: -100px; opacity: 0; transition: opacity 0.3s; }
        .tooltip-container:hover .tooltip-text { visibility: visible; opacity: 1; }
      ")))
    ),
    
    div(
      id = ns("main_container"),
      
      div(
        id = ns("sidebar"),
        h3("Input Car Specifications"),
        
        fluidRow(
          column(6, selectInput(ns("manufacturer"), label = labelWithTooltip("Manufacturer:", "Select car manufacturer."), choices = names(ui_config$manufacturer_models), width = "100%")),
          column(6, uiOutput(ns("model_ui")))
        ),
        fluidRow(
          column(6, sliderInput(ns("year_of_manufacture"), label = labelWithTooltip("Year:", "Year manufactured."), min = 1940, max = 2025, value = 2018, step = 1, sep = "", width = "100%")),
          column(6, sliderInput(ns("mileage"), label = labelWithTooltip("Mileage (km):", "Total distance traveled."), min = 0, max = 800000, value = 80000, step = 500, width = "100%"))
        ),
        fluidRow(
          column(6, selectInput(ns("body_type"), label = labelWithTooltip("Body Type:", "Select car's body style."), choices = ui_config$body_type, width = "100%")),
          column(6, selectInput(ns("transmission"), label = labelWithTooltip("Transmission:", "Select transmission type."), choices = ui_config$transmission, width = "100%"))
        ),
        fluidRow(
          column(6, selectInput(ns("drivetrain"), label = labelWithTooltip("Drivetrain:", "Select drivetrain type."), choices = ui_config$drivetrain, width = "100%")),
          column(6, selectInput(ns("fuel_type"), label = labelWithTooltip("Fuel Type:", "Select fuel type."), choices = ui_config$fuel_type, width = "100%"))
        ),
        fluidRow(
          column(6, selectizeInput(ns("exterior_colour"), label = labelWithTooltip("Exterior Colour:", "Select exterior colour."), choices = ui_config$exterior_colour, width = "100%", options = list(options = exterior_color_data, valueField = 'value', labelField = 'label', searchField = 'label', render = render_js))),
          column(6, selectizeInput(ns("interior_colour"), label = labelWithTooltip("Interior Colour:", "Select interior colour."), choices = ui_config$interior_colour, width = "100%", options = list(options = interior_color_data, valueField = 'value', labelField = 'label', searchField = 'label', render = render_js)))
        ),
        fluidRow(
          column(6, sliderInput(ns("passengers"), label = labelWithTooltip("Passengers:", "Number of seats."), min = 2, max = 14, value = 5, step = 1, width = "100%")),
          column(6, sliderInput(ns("doors"), label = labelWithTooltip("Doors:", "Number of doors."), min = 2, max = 5, value = 4, step = 1, width = "100%"))
        ),
        fluidRow(
          column(6, sliderInput(ns("engine_displacement_L"), label = labelWithTooltip("Displacement (L):", "Engine displacement."), min = 0.6, max = 8.0, value = 2.0, step = 0.1, width = "100%")),
          column(6, sliderInput(ns("engine_cylinders"), label = labelWithTooltip("Cylinders:", "Number of cylinders."), min = 0, max = 16, value = 4, step = 1, width = "100%"))
        ),
        fluidRow(
          column(6, sliderInput(ns("city_consumption"), label = labelWithTooltip("City L/100km:", "Fuel consumption in the city."), min = 0, max = 25, value = 11.0, step = 0.1, width = "100%")),
          column(6, sliderInput(ns("highway_consumption"), label = labelWithTooltip("Highway L/100km:", "Fuel consumption on highway."), min = 0, max = 20, value = 8.5, step = 0.1, width = "100%"))
        ),
        fluidRow(
          column(6, selectInput(ns("engine_type"), label = labelWithTooltip("Engine Type:", "Select engine configuration."), choices = ui_config$engine_type, width = "100%")),
          column(6,
                 h4("Physical Condition assessment"), # A smaller header works better here
                 fileInput(
                   ns("car_images"),
                   label = labelWithTooltip(HTML("Upload Images <strong style='color:red;'>*</strong>"), "Upload one or more photos. This is required."),
                   multiple = TRUE, 
                   accept = c("image/jpeg", "image/png", "image/jpg")
                 )
          )
        ),
        
        
        tags$br(),
        actionButton(ns("submitbutton"), "Predict Price", class = "btn btn-primary btn-lg btn-block")
        
      ),
      
      div(
        id = ns("main_panel"),
        h3('Prediction Output'),
        div(style = "padding: 10px; border-radius: 8px; background-color: rgba(230, 240, 255, 0.9); margin-bottom: 15px;", 
            uiOutput(ns("image_analysis_output"))
        ),
        div(style = "flex: 0 0 auto; padding: 10px; border-radius: 8px; background-color: rgba(245, 245, 245, 0.9);", uiOutput(ns("contents"))),
        hr(),
        div(style = "flex: 1 1 auto; position: relative;", h4("Model Feature Importance", style = "text-align: center;"), plotlyOutput(ns("importance_plot"), height = "95%"))
      )
    )
  )
}


# --- Server Function ---
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
      req(file.exists("ui_config.json"))
      config_data(fromJSON("ui_config.json"))
    })
    
    output$model_ui <- renderUI({
      req(input$manufacturer, config_data(), cancelOutput = TRUE)
      models <- config_data()$manufacturer_models[[input$manufacturer]]
      selectInput(ns("model"), label = labelWithTooltip("Model:", "Select the car model."), choices = models, width = "100%")
    })
    
    trained_model_bundle <- reactiveVal(NULL)
    importance_plot_obj <- reactiveVal(NULL)
    
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
            tags$p(style = "font-weight: bold; text-align: center;", "Image Analysis Result"),
            tags$p(style = "text-align: center;", image_feedback_message)
          )
        })
        
        #  TABULAR DATA PREPARATION ---
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
        
        setProgress(value = 0.9, detail = "Generating price prediction...")
        predictions <- list(
          lower = predict(current_bundle$models$lower, data = newdata, type = "quantiles", quantiles = 0.05)$predictions[,1],
          median = predict(current_bundle$models$median, data = newdata, type = "quantiles", quantiles = 0.50)$predictions[,1],
          upper = predict(current_bundle$models$upper, data = newdata, type = "quantiles", quantiles = 0.95)$predictions[,1]
        )
        
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
              .pred-table { width: 100%; border-collapse: collapse; }
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


