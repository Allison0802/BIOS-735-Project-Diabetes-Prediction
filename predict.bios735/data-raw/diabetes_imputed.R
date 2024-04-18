## code to prepare `diabetes_imputed` dataset goes here
library(mice)
library(caret)
data <- diabetes[, -1]
# split the data
train_index <- createDataPartition(data$diabetes, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# impute training set and test set
train_impute <- train_data |>
  mice(seed = 1349, print = FALSE) |>
  mice::complete("all")

train_imputed1 <- train_impute[[1]]
train_imputed2 <- train_impute[[2]]
train_imputed3 <- train_impute[[3]]
train_imputed4 <- train_impute[[4]]
train_imputed5 <- train_impute[[5]]

use_data(train_imputed1, overwrite = TRUE)
use_data(train_imputed2, overwrite = TRUE)
use_data(train_imputed3, overwrite = TRUE)
use_data(train_imputed4, overwrite = TRUE)
use_data(train_imputed5, overwrite = TRUE)

test_impute <- test_data |>
  mice(seed = 1349, print = FALSE) |>
  mice::complete("all")

test_imputed1 <- test_impute[[1]]
test_imputed2 <- test_impute[[2]]
test_imputed3 <- test_impute[[3]]
test_imputed4 <- test_impute[[4]]
test_imputed5 <- test_impute[[5]]

use_data(test_imputed1, overwrite = TRUE)
use_data(test_imputed2, overwrite = TRUE)
use_data(test_imputed3, overwrite = TRUE)
use_data(test_imputed4, overwrite = TRUE)
use_data(test_imputed5, overwrite = TRUE)

diabetes_complete <- na.omit(data)
use_data(diabetes_complete, overwrite = TRUE)
