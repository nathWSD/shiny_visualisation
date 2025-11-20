
library(tidyverse)
library(caret)
library(ranger)
library(jsonlite)
library(doParallel)
library(foreach)

cat("Setting up file paths...\n")
dir.create("models", showWarnings = FALSE)

MODEL_PATHS <- list(
  lower = "models/ranger_lower.rds",    
  median = "models/ranger_median.rds",
  upper = "models/ranger_upper.rds",
  preproc = "models/ranger_preproc_info.rds"
)
DATA_FILE <- "detailed_car_sales_data_train.csv"
CONFIG_FILE <- "ui_config.json"

if (!file.exists(CONFIG_FILE) || !file.exists(DATA_FILE)) {
  stop("Configuration or Data file not found. Please check paths.")
}
config_data <- fromJSON(CONFIG_FILE)
df <- read.csv(DATA_FILE, stringsAsFactors = TRUE) 


cat("Preparing data preprocessing info from UI config...\n")
preproc_config <- config_data
all_factor_levels <- preproc_config
all_factor_levels$manufacturer_models <- NULL 
all_factor_levels$color_map <- NULL         
all_factor_levels$manufacturer <- names(preproc_config$manufacturer_models)
all_factor_levels$model <- unique(unlist(preproc_config$manufacturer_models))

df_processed <- df
for (col in names(all_factor_levels)) {
  if (col %in% names(df_processed)) {
    df_processed[[col]] <- factor(df_processed[[col]], levels = all_factor_levels[[col]])
  }
}

df_processed <- na.omit(df_processed)

cat("Splitting into train/validation sets...\n")
set.seed(42) 
train_indices <- createDataPartition(df_processed$price, p = 0.8, list = FALSE)
train_data <- df_processed[train_indices, ]
validation_data <- df_processed[-train_indices, ]

cat("Starting parallel model training...\n")


num_cores <- max(1, detectCores() - 1)
cat(paste("Using", num_cores, "cores for training.\n"))
cl <- makeCluster(num_cores)
registerDoParallel(cl)

quantiles_to_train <- c(lower = 0.05, median = 0.50, upper = 0.95)

trained_models <- foreach(
  q_val = quantiles_to_train, 
  q_name = names(quantiles_to_train),
  .packages = 'ranger'
) %dopar% {
  
  cat(paste("Worker", Sys.getpid(), "is training the", q_name, "model (quantile:", q_val, ")...\n"))

  model <- ranger(
    formula = price ~ ., 
    data = train_data,
    quantreg = TRUE,
    num.trees = 2000,       
    mtry = floor(sqrt(ncol(train_data) - 1)), 
    min.node.size = 30,  
    seed = 42,
    importance = 'impurity',
    verbose = FALSE       
  )
  
  return(model)
}

stopCluster(cl)
cat("\nParallel training complete.\n")

names(trained_models) <- names(quantiles_to_train)

cat("Saving trained models...\n")
for (q_name in names(trained_models)) {
  model_path <- MODEL_PATHS[[q_name]]
  saveRDS(trained_models[[q_name]], model_path)
  cat(paste("  - Saved", q_name, "model to", model_path, "\n"))
}

cat("Saving preprocessing information...\n")
preproc_info_to_save <- list(
  all_levels = all_factor_levels 
)
saveRDS(preproc_info_to_save, MODEL_PATHS$preproc)
cat(paste("Preprocessing info SAVED to", MODEL_PATHS$preproc, "\n"))

cat("\n--- Training completed successfully! The app is ready to be run. ---\n")