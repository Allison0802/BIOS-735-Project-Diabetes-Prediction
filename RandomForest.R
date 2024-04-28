#Load packages
library(devtools)
library(caret)
library(ranger)
library(mlr)
load_all("predict.bios735")
set.seed(1)

table(diabetes_train$diabetes)

#Setup
train_d <- subset(diabetes_train,select = -c(site,FPG_final,yr_f, weight, HDL,LDL))
train_d$diabetes <- as.factor(train_d$diabetes)
test_d <- subset(diabetes_test,select = -c(site,FPG_final,yr_f, weight, HDL,LDL))
test_d$diabetes <- as.factor(test_d$diabetes)

library(ranger)
library(caret)

fit.ranger = ranger(
  formula = diabetes ~ ., data = train_d,
  probability = TRUE,
  class.weights = c(1,0.01956133),
  importance = "impurity_corrected"
)

# Make predictions on the test data
predictions_ranger <- predict(fit.ranger, data = test_d)
predicted_classes <- ifelse(predictions_ranger$predictions[,2]> 0.01956133, '1', '0')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_d$diabetes),positive="1")

#Feature importance plot

# Sort the list by absolute values in descending order
sorted_list <- fit.ranger$variable.importance[order(abs(fit.ranger$variable.importance), decreasing = FALSE)]

# Create horizontal dot plot with adjusted margins
par(mar = c(5, 7, 4, 2) + 0.1)  # Set margin
plot(x = sorted_list, y = 1:length(sorted_list), pch = 20, xlim = c(min(sorted_list), max(sorted_list)),
     xlab = "Importance", ylab = "", yaxt = "n", main = "Feature Importance")
axis(2, at = 1:length(sorted_list), labels = names(sorted_list), las = 1)

# Add value text next to each dot
text(x = sorted_list, y = 1:length(sorted_list), labels = round(sorted_list, 2), pos = 4, xpd = TRUE)

#Downsampling
idx.dat <- c(which(train_d$diabetes == '1'),
             sample(which(train_d$diabetes == '0'), 3315))
train_d2 <- train_d[idx.dat,]

fit.ranger2 = ranger(
  formula = diabetes ~ ., data = train_d2,
  probability = TRUE,
  importance = "impurity_corrected"
)

# Make predictions on the test data
predictions_ranger <- predict(fit.ranger2, data = test_d)
predicted_classes <- ifelse(predictions_ranger$predictions[,2]> 0.5, '1', '0')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_d$diabetes),positive="1")

#Feature importance plot

# Sort the list by absolute values in descending order
sorted_list <- fit.ranger2$variable.importance[order(abs(fit.ranger2$variable.importance), decreasing = FALSE)]

# Create horizontal dot plot with adjusted margins
par(mar = c(5, 7, 4, 2) + 0.1)  # Set margin
plot(x = sorted_list, y = 1:length(sorted_list), pch = 20, xlim = c(min(sorted_list), max(sorted_list)),
     xlab = "Importance", ylab = "", yaxt = "n", main = "Feature Importance")
axis(2, at = 1:length(sorted_list), labels = names(sorted_list), las = 1)

# Add value text next to each dot
text(x = sorted_list, y = 1:length(sorted_list), labels = round(sorted_list, 2), pos = 4, xpd = TRUE)

#Comprehensive Tuning Procedure
# hyper_grid <- expand.grid(
#   mtry  = seq(2,16,2),
#   node_size = seq(2,20,2),
#   num.trees = seq(100,1000,50)
# )
# 
# system.time(
#   for(i in 1:nrow(hyper_grid)) {
#     rf <- ranger(
#       formula        = diabetes ~ .,
#       data           = train_d,
#       num.trees      = hyper_grid$num.trees[i],
#       mtry           = hyper_grid$mtry[i],
#       min.node.size  = hyper_grid$node_size[i])
#     # add OOB error to grid
#     hyper_grid$OOB_RMSE[i] <- sqrt(rf$prediction.error)
#   })
