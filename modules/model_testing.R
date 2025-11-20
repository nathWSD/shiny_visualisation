library(tidyverse)
library(ranger)

MODEL_DIR <- "models/"

MODEL_PATHS <- list(
  lower = file.path(MODEL_DIR, "ranger_lower.rds"),
  upper = file.path(MODEL_DIR, "ranger_upper.rds"),
  preproc = file.path(MODEL_DIR, "ranger_preproc_info.rds")
)
TEST_DATA_FILE <- "detailed_car_sales_data_test.csv"

if (!all(sapply(MODEL_PATHS, file.exists))) {
  stop("ERROR: Trained ranger model files not found. Please run the new training script first.")
}

models_list <- list(
  lower = readRDS(MODEL_PATHS$lower),
  upper = readRDS(MODEL_PATHS$upper)
)
preproc_info <- readRDS(MODEL_PATHS$preproc)

cat("Loading and preparing test data...\n")
if (!file.exists(TEST_DATA_FILE)) {
  stop(paste("ERROR: Test data file not found at", TEST_DATA_FILE))
}
df_test_raw <- read.csv(TEST_DATA_FILE)

df_test_processed <- df_test_raw
for (col in names(preproc_info$all_levels)) {
  if (col %in% names(df_test_processed)) {
    df_test_processed[[col]] <- factor(df_test_processed[[col]], levels = preproc_info$all_levels[[col]])
  }
}
df_test_processed <- na.omit(df_test_processed)

actual_prices <- df_test_processed$price

cat("Generating quantile predictions with ranger...\n")

predictions_lower <- predict(
  models_list$lower, 
  data = df_test_processed, 
  type = "quantiles", 
  quantiles = 0.05
)$predictions[, 1]

predictions_upper <- predict(
  models_list$upper, 
  data = df_test_processed, 
  type = "quantiles", 
  quantiles = 0.95
)$predictions[, 1]


cat("Evaluating prediction interval coverage...\n")
test_results <- data.frame(
  Actual_Price = actual_prices,
  Predicted_Lower = predictions_lower,
  Predicted_Upper = predictions_upper
)

test_results$In_Range <- test_results$Actual_Price >= test_results$Predicted_Lower & 
  test_results$Actual_Price <= test_results$Predicted_Upper

cat("\n--- Ranger Model Test Results (90% Confidence Interval) ---\n")

total_entries <- nrow(test_results)
passed_count <- sum(test_results$In_Range)
coverage_rate <- (passed_count / total_entries) * 100

cat(paste("Total Test Entries:", total_entries, "\n"))
cat(paste("Entries Within Range (PASS):", passed_count, "\n"))
cat(paste("Entries Outside Range (FAIL):", total_entries - passed_count, "\n"))
cat(paste("Achieved Coverage Rate:", round(coverage_rate, 2), "%\n"))

if (abs(coverage_rate - 90) < 2.5) { 
  cat("\nVERDICT: PASS. The achieved coverage rate is close to the expected 90%.\n")
} else {
  cat("\nVERDICT: FAIL. The achieved coverage rate deviates significantly from the expected 90%.\n")
}

print(test_results)