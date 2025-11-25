mod_theme_changer_ui <- function(id) {
  ns <- NS(id)
  
  
  theme_choices <- c(
    # --- Light / Standard Themes ---
    "Standard Professional (Cerulean)" = "cerulean",
    "Modern Minimalist (Cosmo)"        = "cosmo",
    "Flat & Clean (Flatly)"            = "flatly",
    "Newspaper Style (Journal)"        = "journal",
    "Sharp & Crisp (Litera)"           = "litera",
    "Bright & Airy (Lumen)"            = "lumen",
    "Elegant & Classic (Lux)"          = "lux",
    "Material Design (Materia)"        = "materia",
    "Fresh Mint (Minty)"               = "minty",
    "Vibrant Purple (Pulse)"           = "pulse",
    "Earthy & Solid (Sandstone)"       = "sandstone",
    "Simple (Simplex)"                 = "simplex",
    "Hand-Drawn / Fun (Sketchy)"       = "sketchy",
    "Silver & Blue (Spacelab)"         = "spacelab",
    "Warm Ubuntu Style (United)"       = "united",
    "Clean Flat Blue (Yeti)"           = "yeti",
    
    # --- Dark Themes ---
    "⚫ Midnight Black (Cyborg)"       = "cyborg",
    "⚫ Standard Dark Mode (Darkly)"   = "darkly",
    "⚫ Muted Grey (Slate)"            = "slate",
    "⚫ Low Contrast Dark (Solar)"     = "solar",
    "⚫ Blue-Grey Dark (Superhero)"    = "superhero"
  )
  
  tagList(
    div(
      class = "well",
      style = "margin-top: 20px;",
      h4("App Appearance"),
      p("Select a new look to update the dashboard style."),
      hr(),
      
      fluidRow(
        # Column 1: Selector
        column(
          width = 6,
          selectInput(
            inputId = ns("theme_selector"),
            label = "Choose a Theme:",
            choices = theme_choices,
            selected = "cerulean"
          )
        ),
        
        # Column 2: Visual Preview
        column(
          width = 6,
          style = "border-left: 1px solid #ddd; padding-left: 15px;",
          tags$label("Theme Preview elements:"),
          div(
            style = "margin-top: 10px;",
            actionButton(ns("dummy_btn"), "Primary Button", class = "btn-primary btn-sm"),
            div(style = "height: 10px;"), # Spacer
            sliderInput(ns("dummy_slider"), NULL, min = 0, max = 100, value = 50, ticks = FALSE)
          )
        )
      )
    )
  )
}

mod_theme_changer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$theme_selector, {
      
      new_theme <- bslib::bs_theme(bootswatch = input$theme_selector, version = 5)
      
      session$setCurrentTheme(new_theme)
      
      showNotification(
        paste("Theme updated to:", input$theme_selector), 
        duration = 2, 
        type = "message"
      )
    })
    
  })
}
