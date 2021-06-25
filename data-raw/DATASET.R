## code to prepare `DATASET` dataset goes here
Data_2002_21 <- read.csv('data-raw/Tournament Data 2002-21.csv')
usethis::use_data(Data_2002_21, overwrite = TRUE)
