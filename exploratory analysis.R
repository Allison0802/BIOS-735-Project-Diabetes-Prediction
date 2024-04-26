library(devtools)
library(usethis)
library(tidyverse)
library(naniar)

install.packages("predict.bios735")
library(predict.bios735)
?diabetes

diabetes$Gender <- factor(diabetes$Gender, level = c(1, 2), labels = c("Male", "Female"))
diabetes$diabetes <- factor(diabetes$diabetes, level = c(0, 1), labels = c("Non-diabetes", "Diabetes"))
diabetes$history <- factor(diabetes$history, level = c(0, 1), labels = c("N", "Y"))

table1::table1(~ Age + Gender + height + weight + BMI + SBP + DBP + Cholesterol 
               + Triglyceride + HDL + LDL + AST + BUN + CCR + smoke + drink + 
                 history | diabetes, data = diabetes, render.continuous=c(.="Mean (SD)"))

diabetes |>
  filter(diabetes == "Diabetes") |>
  select(HDL, LDL, AST, smoke, drink) |>
  naniar::gg_miss_upset()

diabetes |>
  filter(diabetes == "Non-diabetes") |>
  select(HDL, LDL, AST, smoke, drink) |>
  naniar::gg_miss_upset()
