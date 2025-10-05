input_dir <- "../spatial_data"
list.files(input_dir)

library(sf)
library(amc.analysis)

## Read data
mexico_shps_paths <- list.files(path = input_dir, pattern = "*.shp", full.names = TRUE)

## clean data
mexico_shps_raw <- lapply(mexico_shps_paths, sf::st_read)
mexico_shps <- qck_correct_contiguous(mexico_shps_raw)

usethis::use_data(mexico_shps, overwrite = TRUE)
