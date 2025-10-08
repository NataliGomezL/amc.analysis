---
title: "amc.analysis Description"
author: "Natali Stigler-Leguizamo"
date: "2025-10-05"
output:
  html_document:
    keep_md: yes
---



#

'amc.analysis' is an R package for standardizing territorial units over time by accounting for boundary changes. The package detects and groups boundary modifications resulting from territorial mergers and splits using overlap analysis methods. It builds on R functions developed by Stigler (2023), which conceptualize overlapping polygons as a network clustering problem—analogous to the “friends of friends” approach in social networks. The resulting clusters, or aggregated mapping units (AMCs), and their corresponding crosswalk tables facilitate longitudinal analyses that require consistent spatial units across time.

## Installation

To install this package, use:


``` r
remotes::install_github("NataliGomezL/amc.analysis")
```


## Usage

The example is based on the original data from the Mexican state of Campeche, where several municipalities experienced territorial changes between 1995 and 2020


``` r
library(amc.analysis)
library(ggplot2)
data(mexico_shps)
plot <- plot_territorial_units(shps =  mexico_shps,
                               id_unit_var = "cvegeo",
                               period_var = "year",
                               show_legend = FALSE,
                               show_label = TRUE,
                               label_size = 2)

plot + 
  ggplot2::theme(text = element_text(size=6.5)) +
  ggtitle("Overview of raw dataset")+
  labs(subtitle = "3 years of shapefiles, with changes in territorial units from year to year")
```

<img src="README_files/figure-html/plot_territorial_units-1.png" width="100%" />




Step 1: apply function `amc_overlap_analysis()`:


``` r
raw_overlap <- amc_overlap_analysis(shps =  mexico_shps,
                                    id_unit_var = "cvegeo",
                                    period_var = "year")
```

```
## Done for 1995 - 2015
```

```
## Done for 2015 - 2020
```

``` r
raw_overlap
```

```
## # A tibble: 26 × 10
##    dyad                 row_a row_b area_inter  area_a  area_b area_a_overlapped
##    <chr>                <chr> <chr>      [m^2]   [m^2]   [m^2]             <dbl>
##  1 1995_04001 2015_040… 1995… 2015…     2.08e9 2.08e 9 2.08e 9             100.0
##  2 1995_04005 2015_040… 1995… 2015…     1.27e9 1.27e 9 1.27e 9             100.0
##  3 1995_04006 2015_040… 1995… 2015…     7.73e9 1.31e10 7.73e 9              59.1
##  4 1995_04002 2015_040… 1995… 2015…     3.25e9 3.25e 9 3.25e 9             100. 
##  5 1995_04004 2015_040… 1995… 2015…     6.83e9 1.55e10 6.83e 9              44.2
##  6 1995_04008 2015_040… 1995… 2015…     1.07e9 1.07e 9 1.07e 9             100.0
##  7 1995_04003 2015_040… 1995… 2015…     7.11e9 1.28e10 7.11e 9              55.7
##  8 1995_04007 2015_040… 1995… 2015…     2.16e9 2.16e 9 2.16e 9             100.0
##  9 1995_04009 2015_040… 1995… 2015…     4.77e9 4.77e 9 4.77e 9             100.0
## 10 1995_04004 2015_040… 1995… 2015…     8.62e9 1.55e10 1.40e10              55.8
## # ℹ 16 more rows
## # ℹ 3 more variables: area_b_overlapped <dbl>, year_A <dbl>, year_B <dbl>
```


