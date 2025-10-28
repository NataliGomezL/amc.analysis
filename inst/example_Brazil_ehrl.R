#' ---
#' Title: "Run AMC for Brazil"
#' Author: "Matthieu"
#' Date: 2025-10-08
#' ---

library(geobr)
library(sf)
library(tidyverse)

################################
#'## Download data
################################

## check 1
amc_70_10 <- read_comparable_areas(start_year=1970, end_year=2010)
amc_70_10

## download all
years_ehrl <- c(1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2010)


if(!file.exists("../amc_data_tests/amc_moser_ehrl_ALL_geobr.dta")){
  dat_dwnld <- tibble(year=years_ehrl |> head(-1)) |>
    mutate(data=map(year, ~read_comparable_areas(start_year=., end_year=2010)))
  write_rds(dat_dwnld, "../amc_data_tests/amc_moser_ehrl_ALL_geobr.dta")
} else {
  dat_dwnld <- read_rds("../amc_data_tests/amc_moser_ehrl_ALL_geobr.dta")
}


################################
#'## clean output
################################

## get one big table
dat_dwnld_df <- dat_dwnld |>
  mutate(data=map(data, ~st_drop_geometry(.) |> as_tibble())) |>
  rename(year_start=year) |>
  unnest(data)

dat_dwnld_df

## count units
N_byY <- dat_dwnld_df |>
  summarise(n_units=n_distinct(code_amc), .by=year_start)

N_byY |>
  ggplot(aes(x=year_start, y=n_units))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = n_distinct(amc_ehrl$amc))+
  ggtitle("Number of MC units by start year, end year always 2010")


