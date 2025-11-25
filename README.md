# Setup Instructions

To run this application, please copy and paste the following commands sequentially into your **R Console**.

```r
# --- STEP 1: Install Infrastructure ---
install.packages("remotes")
remotes::install_version("remotes", version = "2.5.0", upgrade = "never")

# --- STEP 2: Install Reticulate & Python ---
remotes::install_version("reticulate", version = "1.44.1", upgrade = "never")
library(reticulate)
install_python(version = "3.10")

# --- STEP 3: Setup TensorFlow Environment ---
install.packages("tensorflow")
library(tensorflow)
install_tensorflow(
  version = "2.15", 
  envname = "r-tensorflow", 
  python_version = "3.10"
)

# --- STEP 4: Install Keras & Python Dependencies ---
# We use legacy Keras 2.13.0 for model compatibility
remotes::install_version("keras", version = "2.13.0", upgrade = "never")

# Install required Python libraries into the environment
reticulate::use_virtualenv("r-tensorflow", required = TRUE)
reticulate::py_install(c("pillow", "h5py"), envname = "r-tensorflow")

# --- STEP 5: Install Specific R Packages ---
remotes::install_version("shinyBS", version = "0.61.1", upgrade = "never")
remotes::install_version("thematic", version = "0.1.8", upgrade = "never")
remotes::install_version("ranger", version = "0.17.0", upgrade = "never")

# --- STEP 6: Install Standard Dependencies ---
install.packages(c(
  "shiny", "bslib", "shinyjs", "plotly", "data.table", 
  "readr", "bsicons", "jsonlite", "rstudioapi", "rlang",
  "caret", "Matrix", "dplyr", "RCurl", "randomForest"
))

# --- STEP 7: Launch App ---
# Note: It is recommended to restart your R Session (Ctrl+Shift+F10) before running
shiny::runApp()
