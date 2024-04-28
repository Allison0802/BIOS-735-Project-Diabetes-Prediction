#Load packages
library(devtools)
library(caret)
library(ranger)
library(mlr)
load_all("predict.bios735")

table(diabetes_train$diabetes)

#Setup
train_d <- subset(diabetes_train,select = -c(site,FPG_final,yr_f))
train_d$diabetes <- as.factor(train_d$diabetes)
test_d <- subset(diabetes_test,select = -c(site,FPG_final,yr_f))
test_d$diabetes <- as.factor(test_d$diabetes)

library(caret)
library(ranger)

fit.ranger = ranger(
  formula = diabetes ~ ., data = train_d,
  probability = TRUE,
  class.weights = c(1,0.01956133)
)

# Make predictions on the test data
predictions_ranger <- predict(fit.ranger, data = test_d)
predicted_classes <- ifelse(predictions_ranger$predictions[,2]> 0.01956133, '1', '0')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_d$diabetes),positive="1")

#Humonguous Tuning Procedure
hyper_grid <- expand.grid(
  mtry  = seq(2,16,2),
  node_size = seq(2,20,2),
  num.trees = seq(100,1000,50)
)

system.time(
  for(i in 1:nrow(hyper_grid)) {
    rf <- ranger(
      formula        = diabetes ~ .,
      data           = train_d,
      num.trees      = hyper_grid$num.trees[i],
      mtry           = hyper_grid$mtry[i],
      min.node.size  = hyper_grid$node_size[i])
    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(rf$prediction.error)
  })
