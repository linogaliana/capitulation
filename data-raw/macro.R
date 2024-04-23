## code to prepare `macro` dataset goes here

path_data <- "./../temporary" 

macro <- data.table::data.table(
  wealthyR::import_price(paste0(path_data,"/../capitulation/inst/dataINSEE/Destinie"))
)[,.SD,.SDcols = c('annee','Prix')]


usethis::use_data(macro, overwrite = TRUE)
