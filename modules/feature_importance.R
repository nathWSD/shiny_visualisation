set.seed(7)
#install.packages("mlbench")
#install.packages("caret")
# load the library

library(mlbench)
library(caret)

# load the dataset
data_file_path <- "car_sales_data.csv"

training_data <- read_csv(data_file_path)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
print("model training feature to start")
# train the model
model <- train(price ~ ., data = training_data, method = "rf", preProcess = "scale", trControl = control)

# estimate variable importance
importance <- varImp(model, scale=FALSE)
print("model training feature ended")
saveRDS(model, file = "models/feature_extract_trained_rf_model.rds")

# summarize importance
print(importance)
# plot importance
plot(importance)