install.packages("predict.bios735")
library(devtools)
library(testthat)
library(tidyverse)
library(naniar)
library(reshape2)

load_all()
library(predict.bios735)
document()
build()

?diabetes
?bayes.ridge.mcmc()
?penalized.logit()

# descriptive stats
diabetes$Gender <- factor(diabetes$Gender, level = c(1, 2), labels = c("Male", "Female"))
diabetes$diabetes <- factor(diabetes$diabetes, level = c(0, 1), labels = c("Non-diabetes", "Diabetes"))
diabetes$history <- factor(diabetes$history, level = c(0, 1), labels = c("N", "Y"))

table1::table1(~ Age + Gender + height + weight + BMI + SBP + DBP + Cholesterol 
               + Triglyceride + HDL + LDL + AST + BUN + CCR + smoke + drink + 
                 history | diabetes, data = diabetes, render.continuous=c(.="Mean (SD)"))

# missing patterns
diabetes |>
  select(HDL, LDL, AST, smoke, drink) |>
  naniar::gg_miss_upset()

# correlation heatmap
corr_mat <- round(cor(select(diabetes, -c(id, diabetes, FPG, FPG_final)), use = "complete.obs"),2)
melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "#FFDD95",
                      high = "#4c9cd3",
                      guide = "colorbar") +
  labs(x = "", y = "")

# test
use_test("logistic")
test_package("predict.bios735")
document()
