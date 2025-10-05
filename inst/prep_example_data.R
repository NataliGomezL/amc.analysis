input_dir <- "/home/matifou/Dropbox/Documents/Nati/Mat Nat SAM/stanford/AMC/spatial_data"
library(sf)
mexico_shps_paths <- list.files(path = input_dir, pattern = "*.shp", full.names = TRUE)
mexico_shps <- lapply(mexico_shps_paths, sf::st_read)
usethis::use_data(mexico_shps)
