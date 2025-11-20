library(keras)
library(tensorflow)
library(tfdatasets)
library(magrittr)
library(ggplot2)
library(dplyr)


data_dir <- "C:/Users/Anwender/Downloads/Master/data_ui/car_state/data3a/training" 

# Model hyperparameters
img_height <- 224
img_width <- 224
batch_size <- 32
epochs <- 15

model_save_path <- "C:/Users/Anwender/Downloads/Master/data_ui/models/car_damage_classifier.h5"
class_names_save_path <- "C:/Users/Anwender/Downloads/Master/data_ui/models/car_damage_class_names.txt"
dir.create("models", showWarnings = FALSE)


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

class_names <- train_ds$class_names
cat("Found class names:", paste(class_names, collapse = ", "), "\n")
num_classes <- length(class_names)

train_ds <- train_ds %>% 
  dataset_cache() %>% 
  dataset_shuffle(buffer_size = 1000) %>% 
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)

val_ds <- val_ds %>% 
  dataset_cache() %>% 
  dataset_prefetch(buffer_size = tf$data$AUTOTUNE)


base_model <- application_mobilenet_v2(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(img_height, img_width, 3)
)


freeze_weights(base_model)



input_tensor <- layer_input(shape = c(img_height, img_width, 3))


output_tensor <- input_tensor %>%
  layer_rescaling(scale = 1/127.5, offset = -1) %>% 
  base_model() %>%
  layer_global_average_pooling_2d() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = num_classes, activation = "softmax")


model <- keras_model(inputs = input_tensor, outputs = output_tensor)


model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = c("accuracy")
)

summary(model)


cat("\nStarting model training...\n")

history <- model %>% fit(
  train_ds,
  epochs = epochs,
  validation_data = val_ds
)

cat("\nTraining complete.\n")



plot(history) + 
  labs(title = "Model Training History", y = "Value") +
  theme_minimal()



cat(paste("\nSaving trained model to:", model_save_path, "\n"))
save_model_hdf5(model, filepath =model_save_path )

cat(paste("Saving class names to:", class_names_save_path, "\n"))
writeLines(class_names, class_names_save_path)

cat("Script finished successfully!\n")