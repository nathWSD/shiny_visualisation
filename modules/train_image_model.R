# --- 0. Load Required Libraries ---
library(keras)
library(tensorflow)
library(tfdatasets)
library(magrittr)
library(ggplot2)
library(dplyr)

# --- 1. Configuration & Setup ---

# SET THIS to the path of your main image folder
data_dir <- "C:/Users/Anwender/Downloads/Master/data_ui/car_state/data3a/training" 

# Model hyperparameters
img_height <- 224
img_width <- 224
batch_size <- 32
epochs <- 15

# Path to save the final, trained model
model_save_path <- "C:/Users/Anwender/Downloads/Master/data_ui/models/car_damage_classifier.h5"
class_names_save_path <- "C:/Users/Anwender/Downloads/Master/data_ui/models/car_damage_class_names.txt"
dir.create("models", showWarnings = FALSE)


# --- 2. Create Data Pipelines ---
# Training Dataset
train_ds <- image_dataset_from_directory(
  data_dir,
  validation_split = 0.2,
  subset = "training",
  seed = 123,
  image_size = c(img_height, img_width),
  batch_size = batch_size
)

# Validation Dataset
val_ds <- image_dataset_from_directory(
  data_dir,
  validation_split = 0.2,
  subset = "validation",
  seed = 123,
  image_size = c(img_height, img_width),
  batch_size = batch_size
)

# Get the class names that were automatically found
class_names <- train_ds$class_names
cat("Found class names:", paste(class_names, collapse = ", "), "\n")
num_classes <- length(class_names)

# Improve performance with caching and prefetching
train_ds <- train_ds %>% 
  dataset_cache() %>% 
  dataset_shuffle(buffer_size = 1000) %>% 
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)

val_ds <- val_ds %>% 
  dataset_cache() %>% 
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)


# --- 3. Build the Model using Transfer Learning ---

# Load the pre-trained base model
base_model <- application_mobilenet_v2(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(img_height, img_width, 3)
)

# --- THE FIX IS HERE ---
# Freeze the base model using the new, correct function.
freeze_weights(base_model)
# --- END OF FIX ---

# Define the input layer
input_tensor <- layer_input(shape = c(img_height, img_width, 3))

# Create the new "head" of the model
output_tensor <- input_tensor %>%
  layer_rescaling(scale = 1/127.5, offset = -1) %>% 
  base_model() %>%
  layer_global_average_pooling_2d() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = num_classes, activation = "softmax")

# Combine into the final model
model <- keras_model(inputs = input_tensor, outputs = output_tensor)

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)

summary(model)


# --- 4. Train the Model ---
cat("\nStarting model training...\n")

history <- model %>% fit(
  train_ds,
  epochs = epochs,
  validation_data = val_ds
)

cat("\nTraining complete.\n")


# --- 5. Visualize Training Results ---
plot(history) + 
  labs(title = "Model Training History", y = "Value") +
  theme_minimal()


# --- 6. Save the Final Model ---

cat(paste("\nSaving trained model to:", model_save_path, "\n"))
save_model_hdf5(model, filepath =model_save_path )

cat(paste("Saving class names to:", class_names_save_path, "\n"))
writeLines(class_names, class_names_save_path)

cat("Script finished successfully!\n")