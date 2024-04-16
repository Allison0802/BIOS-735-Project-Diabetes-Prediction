## code to prepare `diabetes` dataset goes here
# Source: https://datadryad.org/stash/downloads/file_stream/78961
library(tidyverse)
if (!file.exists("data-raw/RC Health Care Data-20180820.xlsx")) {
  download.file("https://datadryad.org/stash/downloads/file_stream/78961", "data-raw/RC Health Care Data-20180820.xlsx", mode = 'wb')
}

raw <- readxl::read_xlsx('data-raw/RC Health Care Data-20180820.xlsx')
names(raw) <- gsub("\\s*\\(.*?\\)", "", names(raw))
names(raw) <- c(names(raw)[1:12], "HDL", names(raw)[14:18], "FPG_final", "Diabetes", "censor", "yr_f", "smoke", "drink", "history")
diabetes <- select(raw, -Diabetes) |>
  rename(diabetes = censor)

usethis::use_data(diabetes, overwrite = TRUE)
