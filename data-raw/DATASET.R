## code to prepare `DATASET` dataset goes here
Data_2002_23_withSag <- read.csv('data-raw/Tournament Data 2002-23_plusSagarin.csv')
usethis::use_data(Data_2002_23_withSag, overwrite = TRUE)
