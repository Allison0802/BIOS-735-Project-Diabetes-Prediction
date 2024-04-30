## loading libraries
library(dplyr)
library(glmnet)
library(tidyr)
library(ggplot2)
library(devtools)
library(glmnet)
library(caret)
library(testthat)

## Loading our package
load_all("./BIOS-735-Project-Diabetes-Prediction/predict.bios735")
document("./BIOS-735-Project-Diabetes-Prediction/predict.bios735")

test_file("./BIOS-735-Project-Diabetes-Prediction/predict.bios735/tests/testthat/test-logistic.R")

data("diabetes_train")
data("diabetes_test")

### load the data, and create the design matrix
X = x = scale(model.matrix(diabetes ~ Age + Gender + BMI + SBP + DBP + height + 
                            Triglyceride + ALT + AST + BUN + CCR + Cholesterol +
                            factor(smoke) + factor(drink) + factor(history), data = diabetes_train)[,-1])
y = diabetes_train$diabetes
x_test = scale(model.matrix(diabetes ~ Age + Gender + BMI + SBP + DBP + height + 
                              Triglyceride + ALT + AST + BUN + CCR + Cholesterol +
                              factor(smoke) + factor(drink) + factor(history), data = diabetes_test)[,-1])
y_test = diabetes_test$diabetes

### We get the best lambda
lambda_ <- seq(0.013, 0.0008, by = -0.0001)
cv_lambda = cv.penalized.logit(X,y, n_folds = 5, lambda = lambda_)
cv_lambda$se.1.lambda
cv_lambda$lambda.min

### Glm solution
glm.sol = glm(y ~ x, data = diabetes_train, family = binomial)
summary(glm.sol)

### own solution
beta0 = glm(y~X, family = "binomial")$coefficients
own.logit = penalized.logit(X,y, beta = beta0, lambda = cv_lambda$se.1.lambda) 
own.logit


### path plot ridge,
start = Sys.time()
lambda_ <- c(seq(1,0.01,-0.05),seq(0.01, 0.0008, by = -0.002))
own.logit_all = penalized.logit(X,y, beta = beta0, lambda =  lambda_) 
colnames(own.logit_all) = paste0("X",round(lambda_,4))
df_plogit = data.frame(own.logit_all)
df_plogit$Variable = row.names(own.logit_all)

df_plogit = pivot_longer(df_plogit, cols = -Variable, names_to = "Lambda", values_to = "Estimate") %>% 
  filter(Variable != "(Intercept)") 

df_plogit$lambda = as.numeric(gsub("X", "", df_plogit$Lambda))
df_plogit$Variable = gsub("X", "", df_plogit$Variable)

#ref.values = data.frame("Value" = rep(seq(-.5,1, length = 5),2), 
#                        "Model" = c(rep("cv.glmnet",5),rep("cv.penalized.logit",5)), 
#                        "Lambda" = c(rep(cv_glm$lambda.1se,5),rep(cv_lambda$se.1.lambda,5)))

ggplot(df_plogit, 
      mapping = aes(x = log(lambda), y = Estimate, color = Variable )) + 
    theme_bw() +
   labs(title = "Ridge regression coefficients path plot")
    geom_line() # + 
#    geom_vline(xintercept = log(cv_glm$lambda.1se),color="red", linetype = "dashed") + 
#    geom_vline(xintercept = log(cv_lambda$se.1.lambda),color="blue", linetype = "dashed")
end = Sys.time()
duration=end-start; print(duration)


### Estimating the performance of the model
pred_plogit = ifelse(1/(1+exp(-cbind(1,x_test) %*%own.logit)) > mean(y),1,0)
cMatrix_plogit = caret::confusionMatrix(as.factor(pred_plogit),as.factor(y_test)); cMatrix_plogit

### glmnet solution
### Using glmnet to find a lambda
cv_glm = cv.glmnet(x = X, y = y, family = binomial(link="logit"), alpha = 0, type.measure = "auc") 
cv_glm$lambda.1se
cv_glm$lambda.min

#### fitting the model
net.sol = glmnet(x = cbind(1,X), y = y, family = binomial(link="logit"), 
                 alpha = 0,  
                 lambda = cv_glm$lambda.1se)

##
df.rownames = row.names(cbind(net.sol$beta,own.logit)); df.rownames[1] = "Intercept"
df = data.frame(Variable = df.rownames);
df$glmnet = as.numeric(net.sol$beta)
df$Penalized.Logit = as.numeric(own.logit)
  
long_df <- pivot_longer(df, cols = -Variable, names_to = "Model", values_to = "Estimate") %>% 
            filter(Variable != "Intercept") %>% 
            arrange(desc(abs(Estimate)), .by_group = factor("Model")); long_df

### Now order the 
ggplot(long_df, aes(x = reorder(Variable,-abs(Estimate)), y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Estimate", color = "Model") +
  scale_fill_manual(values = c("glmnet" = "#7BAFD4", "Penalized.Logit" = "#13294B"))



