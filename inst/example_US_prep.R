library(tidyverse)
library(tigris)

# years_tiger <- 1990:2025
years_tiger <- c(1990, 2000, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
                 2020, 2021, 2022, 2023, 2024)

counties_safe <- function(year, ...){
  out <- try(counties(year=year, cb = TRUE), silent = TRUE)
  if(inherits(out, "try-error")){
    warning(paste("Error with", year))
    return(NULL)
  } else{
    print(paste("OK for", year))
    return(out)
  }
}

counties_safe(1991)

tig_dat_shp <- tibble(year=years_tiger) |>
  mutate(data = map(year, ~counties_safe(year=.)))

tig_dat_shp_ok <- tig_dat_shp |>
  filter(!map_lgl(data, is.null))



write_rds(tig_dat_shp_ok, "../amc_data_tests/US_shps_tigris_some.rds")
