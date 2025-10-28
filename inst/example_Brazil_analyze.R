#' ---
#' Title: "Run AMC for Brazil"
#' Author: "Matthieu"
#' Date: 2025-10-08
#' ---

library(amc.analysis)
library(sf)
library(haven)
library(tidyverse)

################################
#'## Read data
################################

## read Ehrl 2017, downloaded from Christian Moser website
amc_ehrl <- read_dta("../amc_data_tests/amc_moser_ehrl.dta")

BRA_over_df <- read_rds("../amc_data_tests/BRAZIL_all_overlap_raw.rds")

################################
#'## Visu
################################

## density
BRA_over_df |>
  filter(area_a_overlapped>2) |>
  filter(year_A %in% c("1872", "1900", "1950", "2000")) |>
  # filter(year_A %in% c("2000")) |>
  ggplot(aes(x=year_A, y=area_a_overlapped))+
  geom_violin()

BRA_over_df |>
  filter(year_A %in% c("1872")) |>
  ggplot(aes(x=area_a_overlapped))+
  geom_histogram()

## mean
over_mean <- BRA_over_df |>
  filter(area_a_overlapped>2) |>
  mutate(max_overlap = pmax(area_a_overlapped, area_b_overlapped)) |>
  summarise(mean_overlap= mean(max_overlap), .by=c(year_A, year_B)) |>
  mutate(across(c(year_A, year_B), as.integer))

ggplot(aes(x=year_A, y=mean_overlap), data=over_mean)+
  geom_line()

