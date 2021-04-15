## code to prepare `DATASET` dataset goes here
Data_2002_19 <- read.csv('data-raw/Tournament Data 2002-19.csv')
usethis::use_data(Data_2002_19, overwrite = TRUE)
