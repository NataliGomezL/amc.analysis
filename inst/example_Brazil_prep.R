#' ---
#' Title: "Run AMC for Brazil"
#' Author: "Matthieu"
#' Date: 2025-10-08
#' ---

library(amc.analysis)
library(tictoc)
library(geobr)
library(sf)
library(haven)
library(tidyverse)

################################
#'## Read data
################################

## read Ehrl 2017, downloaded from Christian Moser website
amc_ehrl <- haven::read_dta("../amc_data_tests/amc_moser_ehrl.dta")

## download brazil shapefiles: do only once!
if(FALSE){

  years_muni <- c(1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2001, 2005, 2007, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

  BRA_shps <- lapply(years_muni, \(x) read_municipality(year = x))
  names(BRA_shps) <- years_muni
  BRA_shps

  saveRDS(BRA_shps, "../amc_data_tests/BRAZIL_all.rds")
}

BRA_shps <- readRDS("../amc_data_tests/BRAZIL_all.rds")

################################
#'## Simple explo
################################


n_ehrl <- n_distinct(amc_ehrl$amc)

## count nrow BRA
nrow_df <- enframe(map_int(BRA_shps, nrow),
                   name = "year", value="n_rows") %>%
  mutate(year=as.integer(year))

## plot
nrow_df %>%
  ggplot(aes(x=year, y=n_rows))+
  geom_hline(yintercept = n_ehrl, color="blue", linetype=2)+
  geom_line()+
  ggtitle("N units over time")+
  labs(subtitle = "Values for Ehrl 2017 in blue")


################################
#'## Prep AMC
################################


## prep data: add year
BRA_shps_c <- map2(BRA_shps, names(BRA_shps), ~mutate(.x, year=.y))

## projection?
st_crs(BRA_shps[[1]]) # this is not projected!

## project
BRA_shps_c_proj <- map2(BRA_shps, names(BRA_shps), ~mutate(.x, year=.y) %>%
                     st_transform("ESRI:102033"))



## issue with duplicated units!?
BRA_shps_c_df <- map_dfr(BRA_shps_c, ~select(., code_muni, year) %>%
                           st_drop_geometry() %>% as_tibble()) %>%
  mutate(year=as.integer(year))

## issue with id 0!?
BRA_shps_c_df %>%
  add_count(code_muni, year) %>%
  filter(n>1) %>%
  count(code_muni, year)

## what is it?
shp_code0 <- map_dfr(BRA_shps_c, ~filter(., code_muni==0) %>%
                       select(code_muni, year))

shp_code0 %>%
  ggplot()+
  geom_sf()+
  facet_wrap(~year)

## temp clean
make_unique <- function(df, id_unit_var){
  df |>
    mutate(n_unit = row_number(), .by= all_of(id_unit_var)) |>
    mutate(n_unit=ifelse(n_unit==1, "", paste0("_", letters[n_unit])),
           !!rlang::ensym(id_unit_var) :=paste0(!!rlang::ensym(id_unit_var), n_unit)) |>
    select(-n_unit)
}

BRA_shps_c_proj_c <- lapply(BRA_shps_c_proj, make_unique, id_unit_var = "code_muni")

## check validity
sapply(BRA_shps_c_proj_c, \(x) all(st_is_valid(x)))

## make valid
BRA_shps_c_proj_c2 <- lapply(BRA_shps_c_proj_c, st_make_valid)

################################
#'## Run AMC
################################

## select a few to start:
BRA_shps_short_proj <- BRA_shps_c_proj_c[c("1872",  "2000", "2024")]

map_int(BRA_shps_short, nrow)

## run all!
tic()
over_df <- amc_overlap_analysis(BRA_shps_c_proj_c2,
                                period_var = "year", id_unit_var="code_muni")
toc()

ntot <- sum(sapply(BRA_shps_short_proj, nrow))


## save file
write_rds(over_df, "../amc_data_tests/BRAZIL_all_overlap_raw.rds")
