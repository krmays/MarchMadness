## code to prepare `DATASET` dataset goes here
Data_2002_23 <- read.csv('data-raw/Tournament Data 2002-23.csv')
usethis::use_data(Data_2002_23, overwrite = TRUE)
