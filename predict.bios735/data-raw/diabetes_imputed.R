## code to prepare `diabetes_imputed` dataset goes here
library(mice)
library(caret)
data <- diabetes[, -1]
# split the data
set.seed(1349)
train_index <- createDataPartition(data$diabetes, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# impute training set and test set
diabetes_train <- mice(data = train_data, seed = 1349, print = FALSE, m = 1) |> complete()
use_data(diabetes_train, overwrite = TRUE)

diabetes_test <- mice(data = test_data, seed = 1349, print = FALSE, m = 1) |> complete()
use_data(diabetes_test, overwrite = TRUE)

diabetes_complete <- na.omit(data)
use_data(diabetes_complete, overwrite = TRUE)
