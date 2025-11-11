library(tidyverse)
library(Matrix)
library(xgboost)
library(jsonlite)

MODEL_PATHS <- list(
  lower = "models/xgb_lower.xgb", 
  median = "models/xgb_median.xgb", 
  upper = "models/xgb_upper.xgb", 
  preproc = "models/xgb_preproc_info.rds"
)
TEST_DATA_FILE <- "detailed_car_sales_data_test.csv"

cat("Loading trained models and preprocessing info...\n")
if (!all(sapply(MODEL_PATHS, file.exists))) {
  stop("ERROR: Trained model files not found. Please run the training script first.")
}

models_list <- list(
  lower = xgb.load(MODEL_PATHS$lower),
  upper = xgb.load(MODEL_PATHS$upper)
)
preproc_info <- readRDS(MODEL_PATHS$preproc)

cat("Loading test data...\n")
if (!file.exists(TEST_DATA_FILE)) {
  stop(paste("ERROR: Test data file not found at", TEST_DATA_FILE))
}
df_test_raw <- read.csv(TEST_DATA_FILE)

actual_prices <- df_test_raw$price

cat("Preprocessing test data...\n")
df_test_processed <- df_test_raw %>%
  # Use the factor levels saved from the training data
  mutate(across(all_of(names(preproc_info$all_levels)), 
                ~factor(., levels = preproc_info$all_levels[[cur_column()]]))) %>%

  na.omit() 

actual_prices_processed <- df_test_processed$price

cat("Preparing DMatrix for prediction...\n")

test_matrix_small <- sparse.model.matrix(price ~ . -1, data = df_test_processed)

train_features <- preproc_info$feature_names
test_features <- colnames(test_matrix_small)

missing_cols <- setdiff(train_features, test_features)

if (length(missing_cols) > 0) {
  missing_matrix <- Matrix(0, nrow = nrow(test_matrix_small), ncol = length(missing_cols), 
                           dimnames = list(NULL, missing_cols), sparse = TRUE)
  
  test_matrix_full <- cbind(test_matrix_small, missing_matrix)
  
  pred_matrix_final <- test_matrix_full[, train_features, drop = FALSE]
} else {
  pred_matrix_final <- test_matrix_small[, train_features, drop = FALSE]
}

dtest <- xgb.DMatrix(data = pred_matrix_final)


# --- 5. Generate Predictions ---
cat("Generating quantile predictions...\n")
predictions_lower <- predict(models_list$lower, dtest)
predictions_upper <- predict(models_list$upper, dtest)

# --- Test Coverage  ---

# Create a data frame for results
test_results <- data.frame(
  Actual_Price = actual_prices_processed,
  Predicted_Lower = predictions_lower,
  Predicted_Upper = predictions_upper
)

# Check if the actual price falls within the [Lower, Upper] range
test_results$In_Range <- test_results$Actual_Price >= test_results$Predicted_Lower & 
  test_results$Actual_Price <= test_results$Predicted_Upper

cat("\n--- Model Test Results (90% Confidence Interval) ---\n")

total_entries <- nrow(test_results)
passed_count <- sum(test_results$In_Range)
coverage_rate <- (passed_count / total_entries) * 100

cat(paste("Total Test Entries:", total_entries, "\n"))
cat(paste("Entries Within Range (PASS):", passed_count, "\n"))
cat(paste("Entries Outside Range (FAIL):", total_entries - passed_count, "\n"))
cat(paste("Achieved Coverage Rate:", round(coverage_rate, 2), "%\n"))

# The expected coverage rate for a 5% to 95% quantile model is 90%.
if (abs(coverage_rate - 90) < 2) {
  cat("\nVERDICT: PASS. The achieved coverage rate is close to the expected 90%.\n")
} else {
  cat("\nVERDICT: FAIL. The achieved coverage rate deviates significantly from the expected 90%.\n")
}

print(head(test_results))