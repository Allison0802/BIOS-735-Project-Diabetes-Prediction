#' Diabetes data
#'
#' This dataset is the output of a Chinese research study conducted in 2016 with a
#' median follow-up time of 3.1 years. Participants were free of diabetes at baseline
#' and a total of 4174 participants have developed diabetes by the end of the study.
#'
#' @format ## `diabetes`
#' A data frame with 4,303 rows and 18 columns:
#' \describe{
#'   \item{id}{study id for every individual}
#'   \item{Age}{age of the individual in years}
#'   \item{Gender}{gender of the individual; 1: Male, 2: Female}
#'   \item{site}{32 sites in total}
#'   \item{height}{height in cm}
#'   \item{weight}{height in kg}
#'   \item{BMI}{Body Mass Index in kg/m^2}
#'   \item{SBP}{systolic blood pressure in mmHg}
#'   \item{DBP}{diastolic blood pressure in mmHg}
#'   \item{FPG}{Fasting Plasma Glucose in mmol/L}
#'   \item{Cholesterol}{cholesterol level in mmol/L}
#'   \item{Triglyceride}{triglycerides level in mmol/L}
#'   \item{HDL}{High-Density Lipoprotein Cholesterol level in mmol/L}
#'   \item{LDL}{Low-Density Lipoprotein level in mmol/L}
#'   \item{ALT}{Alanine Aminotransferase level in U/L}
#'   \item{AST}{Aspartate Aminotransferase level in U/L}
#'   \item{BUN}{Blood Urea Nitrogen level in mmol/L}
#'   \item{CCR}{Creatinine Clearance level in umol/L}
#'   \item{FPG_final}{Fasting Plasma Glucoseof the final visit in mmol/L}
#'   \item{diabetes}{diabetes diagnosis by the end of the study; 1: Diabetes, 0: Non-diabetes}
#'   \item{yr_f}{year of follow-up}
#'   \item{smoke}{smoking status; 1: Current Smoker, 2: Ever Smoker, 3: Never Smoker}
#'   \item{drink}{drinking status; 1: Current Drinker, 2: Ever Drinker, 3: Never Drinker}
#'   \item{history}{family histroy of diabetes; 1: Yes, 0: No}
#'   ...
#' }
#' @source <https://datadryad.org/stash/dataset/doi:10.5061/dryad.ft8750v>
"diabetes"
