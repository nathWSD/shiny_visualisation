
#install.packages("shinyjs")



labelWithTooltip <- function(labelText, tooltipText) {
  tags$label(
    labelText,
    bslib::tooltip(
      shiny::icon("info-circle", style = "margin-left: 5px; color: #007bff;"),
      tooltipText,
      placement = "right"
    )
  )
}


mod_prediction_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(paste0("
        html, body { height: 100%; width: 100%; margin: 0; padding: 0; overflow: hidden; }
        body {
          background-size: cover; background-position: center center;
          background-repeat: no-repeat; background-attachment: fixed;
          transition: background-image 1s ease-in-out;
        }
        .container-fluid { padding-top: 20px; padding-bottom: 20px; }
        #", ns("sidebar"), ", #", ns("main_panel"), " {
          background-color: rgba(255, 255, 255, 0.85); border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); padding: 20px;
        }
        #", ns("main_panel"), " { min-height: 80vh; }
        #", ns("output_well"), " {
          background-color: rgba(245, 245, 245, 0.9); border: 1px solid #e3e3e3;
          padding: 15px; border-radius: 8px;
        }
      ")))
    ),
    
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar"),
        HTML("<h3>Input parameters</h3>"),
        
        selectInput(ns("Manufacturer"),
                    label = labelWithTooltip("Manufacturer:", "Select the car manufacturer from the list."),
                    choices = c("Ford", "Porsche", "Toyota", "VW", "BMW")),
        
        uiOutput(ns("Model_ui")),
        
        sliderInput(ns("engine_size"),
                    label = labelWithTooltip("Engine size:", "Specify the engine displacement in liters."),
                    min = 1.0, max = 6.0, value = 1.0, step = 0.2),
        
        sliderInput(ns("year_of_manufacture"),
                    label = labelWithTooltip("Year of manufacture:", "Choose the year the car was manufactured."),
                    min = 1980, max = 2025, value = 2000, step = 1, sep = ""),
        
        selectInput(ns("fuel_type"),
                    label = labelWithTooltip("Fuel type:", "Select the car's fuel type."),
                    choices = list("Petrol" = "petrol", "Diesel" = "diesel", "Hybrid" = "hybrid")),
        
        sliderInput(ns("mileage"),
                    label = labelWithTooltip("Mileage:", "Enter the total distance the car has traveled."),
                    min = 0, max = 500000, value = 100000, step = 1),
        
        actionButton(ns("submitbutton"), "Submit", class = "btn btn-primary")
      ),
      mainPanel(
        id = ns("main_panel"),
        tags$label(h3('Status/Output')),
        wellPanel(
          id = ns("output_well"),
          uiOutput(ns("contents")),
          tableOutput(ns("tabledata"))
        )
      )
    )
  )
}

mod_prediction_panel_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    manufacturer_models <- list(
      "Ford" = c("Fiesta", "Mondeo", "Focus"),
      "Porsche" = c("718 Cayman", "911", "Cayenne"),
      "Toyota" = c("RAV4", "Prius", "Yaris"),
      "VW" = c("Polo", "Golf", "Passat"),
      "BMW" = c("Z4", "M5", "X3")
    )
    
    output$Model_ui <- renderUI({
      selectInput(ns("Model"),
                  label = labelWithTooltip("Model:", "Select the specific car model for the chosen manufacturer."),
                  choices = manufacturer_models[[input$Manufacturer]])
    })
    
    
    trained_model <- reactiveVal(NULL)
    
    observe({
      req(input$Manufacturer, input$Model)
      image_path <- paste0("images/", input$Manufacturer, "/", input$Model, ".jpg")
      css_background_url <- paste0("url('", image_path, "')")
      
      shinyjs::runjs(sprintf(
        "$('body').css('background-image', \"%s\")",
        css_background_url
      ))
    })
    
    observeEvent(input$submitbutton, {
      output$contents <- renderUI({ NULL })
      output$tabledata <- renderTable({ NULL })
      
      withProgress(message = 'Processing', style = "old", value = 0, {
        
        # --- MODEL LOADING AND TRAINING LOGIC (RESTORED) ---
        setProgress(value = 0.1, detail = "Locating model...")
        Sys.sleep(0.5)
        
        model_path <- "models/rf_model.rds"
        
        if (file.exists(model_path)) {
          if (is.null(trained_model())) { 
            setProgress(value = 0.2, detail = "Loading existing model...")
            model_bundle <- readRDS(model_path)
            trained_model(model_bundle)
            Sys.sleep(0.5)
          }
        } else {
          setProgress(value = 0.2, detail = "No model found. Training new model...")
          req(shared_data()) 
          df <- shared_data()
          
          # Prepare data for training 
          all_factor_levels <- list(
            manufacturer = tolower(names(manufacturer_models)),
            model = tolower(unlist(manufacturer_models, use.names = FALSE)),
            fuel_type = c("petrol", "diesel", "hybrid")
          )
          df$manufacturer <- factor(tolower(trimws(df$manufacturer)), levels = all_factor_levels$manufacturer)
          df$model <- factor(tolower(trimws(df$model)), levels = all_factor_levels$model)
          df$fuel_type <- factor(tolower(trimws(df$fuel_type)), levels = all_factor_levels$fuel_type)
          
          # Train the Random Forest model
          rf <- randomForest(price ~ ., data = df, ntree = 500, importance = TRUE, na.action = na.omit)
          
          model_bundle <- list(model = rf, factor_levels = all_factor_levels)
          
          # Save the new model to disk
          dir.create("models", showWarnings = FALSE) 
          saveRDS(model_bundle, model_path)
          
          # Store the newly trained model 
          trained_model(model_bundle)
          setProgress(value = 0.6, detail = "Model trained and saved.")
          Sys.sleep(1)
        }
        
        # --- PREDICTION STEP (uses the loaded or newly trained model) ---
        setProgress(value = 0.7, detail = "Preparing prediction...")
        
        # Clean and prepare user inputs
        manufacturer_input <- tolower(trimws(input$Manufacturer))
        model_input <- tolower(trimws(input$Model))
        fuel_input <- tolower(trimws(input$fuel_type))
        
        levels <- trained_model()$factor_levels
        newdata <- data.frame(
          manufacturer = factor(manufacturer_input, levels = levels$manufacturer),
          model = factor(model_input, levels = levels$model),
          engine_size = as.numeric(input$engine_size),
          year_of_manufacture = as.integer(input$year_of_manufacture),
          fuel_type = factor(fuel_input, levels = levels$fuel_type),
          mileage = as.integer(input$mileage)
        )
        
        # Run prediction
        setProgress(value = 0.9, detail = "Predicting...")
        pred <- predict(trained_model()$model, newdata)
        
        setProgress(value = 1, detail = "Done!")
        Sys.sleep(0.5)
        
        # --- RENDER OUTPUT ---
        output$contents <- renderUI({
          tags$h4(paste0("Predicted Price: ", round(pred, 2)))
        })
        
        output$tabledata <- renderTable({
          data.frame(Predicted_Price = round(pred, 2))
        })
      })
    }) 
  })
}