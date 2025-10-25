# Helper function for UI tooltips (no changes)
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


# --- mod_prediction_panel_ui Function (Corrected for Uniform Width) ---

mod_prediction_panel_ui <- function(id) {
  ns <- NS(id)
  
  config_path <- "ui_config.json"
  if (file.exists(config_path)) {
    ui_config <- fromJSON(config_path)
    color_css_map <- ui_config$color_map
  } else {
    # Dummy data to prevent crashing if file is missing
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
        /* CSS (no changes) */
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
          # --- CORRECTED: Added width = "100%" to both selectizeInput calls ---
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
          column(6, sliderInput(ns("city_consumption"), label = labelWithTooltip("City L/100km:", "Fuel consumption in the city."), min = 2, max = 25, value = 11.0, step = 0.1, width = "100%")),
          column(6, sliderInput(ns("highway_consumption"), label = labelWithTooltip("Highway L/100km:", "Fuel consumption on highway."), min = 0, max = 20, value = 8.5, step = 0.1, width = "100%"))
        ),
        fluidRow(
          column(6, selectInput(ns("engine_type"), label = labelWithTooltip("Engine Type:", "Select engine configuration."), choices = ui_config$engine_type, width = "100%"))
        ),
        
        tags$br(),
        actionButton(ns("submitbutton"), "Predict Price", class = "btn btn-primary btn-lg btn-block")
      ),
      
      div(
        id = ns("main_panel"),
        h3('Prediction Output'),
        div(style = "flex: 0 0 auto; padding: 10px; border-radius: 8px; background-color: rgba(245, 245, 245, 0.9);", uiOutput(ns("contents"))),
        hr(),
        div(style = "flex: 1 1 auto; position: relative;", h4("Model Feature Importance", style = "text-align: center;"), plotlyOutput(ns("importance_plot"), height = "95%"))
      )
    )
  )
}


# --- Server Function (Corrected for Background Image and Empty Plot) ---

mod_prediction_panel_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Load config from JSON once in the server ---
    config_data <- reactiveVal(NULL)
    observe({
      req(file.exists("ui_config.json"))
      config_data(fromJSON("ui_config.json"))
    })
    
    # --- CORRECTED: Background image logic ---
    autoInvalidate <- reactiveTimer(5000)
    observe({
      autoInvalidate()
      # Path now points to the correct local directory at the app's root
      images_path <- "detailed_images"
      if (dir.exists(images_path)) {
        all_images <- list.files(images_path, recursive = TRUE, pattern = "\\.(jpg|jpeg|png)$")
        if (length(all_images) > 0) {
          random_image <- sample(all_images, 1)
          # The URL path must match the prefix from addResourcePath in app.R
          js_path <- file.path("detailed_images", random_image)
          # Ensure forward slashes for the URL
          js_path <- gsub("\\\\", "/", js_path)
          shinyjs::runjs(sprintf("$('body').css('background-image', \"url('%s')\")", js_path))
        }
      }
    })
    
    # Dynamic model UI
    output$model_ui <- renderUI({
      req(input$manufacturer, config_data())
      models <- config_data()$manufacturer_models[[input$manufacturer]]
      selectInput(ns("model"), label = labelWithTooltip("Model:", "Select the car model."), choices = models, width = "100%")
    })
    
    trained_model_bundle <- reactiveVal(NULL)
    importance_plot_obj <- reactiveVal(NULL)
    quantile_loss_obj <- function(alpha) { function(preds, dtrain) { labels <- getinfo(dtrain, "label"); grad <- ifelse(labels - preds > 0, -alpha, (1 - alpha)); hess <- rep(1, length(labels)); list(grad = grad, hess = hess) } }
    
    observeEvent(input$submitbutton, {
      req(input$model, cancelOutput = TRUE)
      
      withProgress(message = 'Processing Request', style = "old", value = 0, {
        
        model_paths <- list(lower = "models/xgb_lower.xgb", median = "models/xgb_median.xgb", upper = "models/xgb_upper.xgb", preproc = "models/xgb_preproc_info.rds")
        
        if (all(sapply(model_paths, file.exists))) {
          if(is.null(trained_model_bundle())) {
            bundle <- list(models = lapply(model_paths[c("lower", "median", "upper")], xgb.load), preproc_info = readRDS(model_paths$preproc))
            trained_model_bundle(bundle)
          }
        } else {
          setProgress(value = 0.2, detail = "Models not found. Training...")
          req(shared_data(), config_data()) 
          df <- shared_data()
          
          preproc_config <- config_data()
          all_factor_levels <- preproc_config
          all_factor_levels$manufacturer_models <- NULL
          all_factor_levels$color_map <- NULL
          
          all_factor_levels$manufacturer <- names(preproc_config$manufacturer_models)
          all_factor_levels$model <- unique(unlist(preproc_config$manufacturer_models))
          
          df_processed <- df %>%
            mutate(across(all_of(names(all_factor_levels)), ~factor(., levels = all_factor_levels[[cur_column()]]))) %>%
            na.omit()
          
          train_indices <- createDataPartition(df_processed$price, p = 0.8, list = FALSE)
          train_data <- df_processed[train_indices, ]
          validation_data <- df_processed[-train_indices, ]
          
          train_matrix <- sparse.model.matrix(price ~ . -1, data = train_data)
          dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$price)
          
          validation_matrix <- sparse.model.matrix(price ~ . -1, data = validation_data)
          dvalid <- xgb.DMatrix(data = validation_matrix, label = validation_data$price)
          
          watchlist <- list(train = dtrain, validation = dvalid)
          xgb_params <- list(booster = "gbtree", eta = 0.05, max_depth = 10, eval_metric = "mae")
          
          models_list <- list()
          quantiles_to_train <- c(lower = 0.05, median = 0.50, upper = 0.95)
          dir.create("models", showWarnings = FALSE)
          
          for (i in seq_along(quantiles_to_train)) {
            q_name <- names(quantiles_to_train)[i]; q_val <- quantiles_to_train[[i]]
            model <- xgb.train(params = xgb_params, data = dtrain, nrounds = 5000, objective = quantile_loss_obj(q_val), watchlist = watchlist, early_stopping_rounds = 50, verbose = 0)
            xgb.save(model, model_paths[[q_name]])
            models_list[[q_name]] <- model
          }
          
          preproc_info_to_save <- list(feature_names = colnames(train_matrix), all_levels = all_factor_levels)
          saveRDS(preproc_info_to_save, model_paths$preproc)
          trained_model_bundle(list(models = models_list, preproc_info = preproc_info_to_save))
        }
        
        setProgress(value = 0.9, detail = "Preparing new data for prediction...")
        
        current_bundle <- trained_model_bundle()
        preproc_info <- current_bundle$preproc_info
        
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
          price = 0
        )
        
        for (col in names(preproc_info$all_levels)) {
          if (col %in% names(newdata)) {
            newdata[[col]] <- factor(newdata[[col]], levels = preproc_info$all_levels[[col]])
          }
        }
        
        pred_matrix_small <- sparse.model.matrix(price ~ . -1, data = newdata)
        
        missing_cols <- setdiff(preproc_info$feature_names, colnames(pred_matrix_small))
        if (length(missing_cols) > 0) {
          missing_matrix <- Matrix(0, nrow = 1, ncol = length(missing_cols), dimnames = list(NULL, missing_cols), sparse = TRUE)
          pred_matrix_full <- cbind(pred_matrix_small, missing_matrix)
          pred_matrix_final <- pred_matrix_full[, preproc_info$feature_names, drop = FALSE]
        } else {
          pred_matrix_final <- pred_matrix_small[, preproc_info$feature_names, drop = FALSE]
        }
        
        dtest <- xgb.DMatrix(data = pred_matrix_final)
        predictions <- lapply(current_bundle$models, predict, dtest)
        
        # --- CORRECTED: Plotting logic is now more robust ---
        imp_data <- xgb.importance(model = current_bundle$models$median)
        if (nrow(imp_data) > 0) {
          p <- plot_ly(data = imp_data %>% head(15) %>% arrange(Gain), x = ~Gain, y = ~factor(Feature, levels = Feature), type = 'bar', orientation = 'h') %>%
            layout(title = "", yaxis = list(title = ""), xaxis = list(title = "Feature Importance (Gain)"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
          importance_plot_obj(p)
        } else {
          # If no importance data, set plot to NULL to clear the output
          importance_plot_obj(NULL)
        }
        
        # Detailed prediction output table
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
      # req will prevent rendering if the plot object is NULL
      req(importance_plot_obj())
      importance_plot_obj()
    })
  })
}



