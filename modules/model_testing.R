# --- 0. Load Required Libraries ---
cat("Loading libraries...\n")
# Ensure these libraries are installed: install.packages(c("tidyverse", "ranger"))
library(tidyverse)
library(ranger)

# --- 1. Define File Paths and Load Models ---
cat("Setting up file paths...\n")
MODEL_DIR <- "models/"

# Update paths to point to the new ranger models and preproc file
MODEL_PATHS <- list(
  lower = file.path(MODEL_DIR, "ranger_lower.rds"),
  upper = file.path(MODEL_DIR, "ranger_upper.rds"),
  preproc = file.path(MODEL_DIR, "ranger_preproc_info.rds")
)
TEST_DATA_FILE <- "detailed_car_sales_data_test.csv"

cat("Loading trained ranger models and preprocessing info...\n")
if (!all(sapply(MODEL_PATHS, file.exists))) {
  stop("ERROR: Trained ranger model files not found. Please run the new training script first.")
}

# Load the ranger model objects using readRDS
models_list <- list(
  lower = readRDS(MODEL_PATHS$lower),
  upper = readRDS(MODEL_PATHS$upper)
)
# This RDS file now only contains the list of factor levels
preproc_info <- readRDS(MODEL_PATHS$preproc)

# --- 2. Load and Prepare Test Data (MUCH SIMPLER) ---
cat("Loading and preparing test data...\n")
if (!file.exists(TEST_DATA_FILE)) {
  stop(paste("ERROR: Test data file not found at", TEST_DATA_FILE))
}
df_test_raw <- read.csv(TEST_DATA_FILE)

# This is the ONLY preprocessing step needed.
# It ensures the test data factors have the exact same levels as the training data.
df_test_processed <- df_test_raw
for (col in names(preproc_info$all_levels)) {
  if (col %in% names(df_test_processed)) {
    df_test_processed[[col]] <- factor(df_test_processed[[col]], levels = preproc_info$all_levels[[col]])
  }
}
df_test_processed <- na.omit(df_test_processed)

# Store actual prices for later evaluation
actual_prices <- df_test_processed$price

# --- 3. Generate Predictions with Ranger (MUCH SIMPLER) ---
# No more DMatrix, no more sparse.model.matrix, no more column alignment.
# Ranger works directly with the prepared data frame.
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

# --- 4. Evaluate Coverage (UNCHANGED) ---
# This part of the logic remains exactly the same as it's model-agnostic.
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

if (abs(coverage_rate - 90) < 2.5) { # Allow a small tolerance
  cat("\nVERDICT: PASS. The achieved coverage rate is close to the expected 90%.\n")
} else {
  cat("\nVERDICT: FAIL. The achieved coverage rate deviates significantly from the expected 90%.\n")
}

print(test_results)