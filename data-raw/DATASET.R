## code to prepare `DATASET` dataset goes here
Data_2002_22 <- read.csv('data-raw/Tournament Data 2002-22.csv')
usethis::use_data(Data_2002_22, overwrite = TRUE)
