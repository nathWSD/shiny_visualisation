# Setup Instructions

To run this application, please copy and paste the following commands sequentially into your **R Console**.

```r

# System Structure 

<img width="2122" height="1505" alt="shiny_app" src="https://github.com/user-attachments/assets/531806be-147c-4827-8cfa-8babd98cf070" />
<img width="2122" height="1505" alt="shiny_app" src="https://github.com/user-attachments/assets/531806be-147c-4827-8cfa-8babd98cf070" />


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
reticulate::py_install(c("pillow", "h5py"), envname = "r-tensorf<img width="2122" height="1505" alt="shiny_app" src="https://github.com/user-attachments/assets/b640c7a7-389d-4a2f-b08b-e9e9c1a49d94" />
<img width="2122" height="1505" alt="shiny_app" src="https://github.com/user-attachments/assets/b640c7a7-389d-4a2f-b08b-e9e9c1a49d94" />
low")

# --- STEP 5: Install Specific R Packages ---
remotes::install_version("shinyBS", version = "0.61.1", upgrade = "never")
remotes::install_version("thematic", version = "0.1.8", upgrade = "never")
remotes::install_version("ranger", version = "0.17.0", upgrade = "never")
remotes::install_version("tfdatasets", version = "2.18.0", upgrade = "never")

# --- STEP 6: Install Standard Dependencies ---
install.packages("shiny")
install.packages("bslib")
install.packages("shinyjs")
install.packages("plotly")
install.packages("data.table")
install.packages("readr")
install.packages("bsicons")
install.packages("jsonlite")
install.packages("rstudioapi")
install.packages("rlang")
install.packages("caret")
install.packages("Matrix")
install.packages("dplyr")
install.packages("RCurl")
install.packages("randomForest")

# --- STEP 7: Launch App ---
# Note: restart your R Session (Ctrl+Shift+F10) before running
shiny::runApp()



