
library(tidyverse)
library(caret)
library(Matrix)
library(xgboost)
library(jsonlite)

MODEL_PATHS <- list(
  lower = "models/xgb_lower.xgb", 
  median = "models/xgb_median.xgb", 
  upper = "models/xgb_upper.xgb", 
  preproc = "models/xgb_preproc_info.rds"
)
DATA_FILE <- "detailed_car_sales_data_train.csv"
CONFIG_FILE <- "ui_config.json" 

quantile_loss_obj <- function(alpha) { 
  function(preds, dtrain) { 
    labels <- getinfo(dtrain, "label")
    # Gradient for quantile loss
    grad <- ifelse(labels - preds > 0, -alpha, (1 - alpha))
    # Hessian 
    hess <- rep(1, length(labels))
    list(grad = grad, hess = hess) 
  } 
}

cat("Loading data and config...\n")
if (!file.exists(CONFIG_FILE) || !file.exists(DATA_FILE)) {
  stop("Configuration or Data file not found. Please check paths in run_training.R.")
}
config_data <- fromJSON(CONFIG_FILE)
df <- read.csv(DATA_FILE) 

cat("Preparing data preprocessing info...\n")
preproc_config <- config_data
all_factor_levels <- preproc_config
all_factor_levels$manufacturer_models <- NULL
all_factor_levels$color_map <- NULL
all_factor_levels$manufacturer <- names(preproc_config$manufacturer_models)
all_factor_levels$model <- unique(unlist(preproc_config$manufacturer_models))

cat("Preprocessing data...\n")
df_processed <- df %>%
  mutate(across(all_of(names(all_factor_levels)), ~factor(., levels = all_factor_levels[[cur_column()]]))) %>%
  na.omit()

cat("Splitting into train/validation sets...\n")
set.seed(42) 
train_indices <- createDataPartition(df_processed$price, p = 0.8, list = FALSE)
train_data <- df_processed[train_indices, ]
validation_data <- df_processed[-train_indices, ]

# ---  Create DMatrix for XGBoost ---
cat("Creating XGBoost matrices...\n")
train_matrix <- sparse.model.matrix(price ~ . -1, data = train_data)
dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$price)
validation_matrix <- sparse.model.matrix(price ~ . -1, data = validation_data)
dvalid <- xgb.DMatrix(data = validation_matrix, label = validation_data$price)

watchlist <- list(train = dtrain, validation = dvalid)
xgb_params <- list(booster = "gbtree", eta = 0.05, max_depth = 1000, eval_metric = "mae")

# --- Train and Save Models ---
cat("Starting model training...\n")
quantiles_to_train <- c(lower = 0.05, median = 0.50, upper = 0.95)
dir.create("models", showWarnings = FALSE)

for (i in seq_along(quantiles_to_train)) {
  q_name <- names(quantiles_to_train)[i]; q_val <- quantiles_to_train[[i]]
  cat(paste("Training", q_name, "model (quantile:", q_val, ")... "))
  
  model <- xgb.train(
    params = xgb_params, 
    data = dtrain, 
    nrounds = 50000, 
    objective = quantile_loss_obj(q_val),
    watchlist = watchlist, 
    print_every_n = 1L,
    early_stopping_rounds = 50, 
    verbose = 1
  )
  
  xgb.save(model, MODEL_PATHS[[q_name]])
  cat("SAVED.\n")
}

cat("Saving preprocessing information...\n")
preproc_info_to_save <- list(
  feature_names = colnames(train_matrix), 
  all_levels = all_factor_levels 
)
saveRDS(preproc_info_to_save, MODEL_PATHS$preproc)
cat("Preprocessing info SAVED.\n")

cat("Training completed successfully! App is ready to be run.\n")

