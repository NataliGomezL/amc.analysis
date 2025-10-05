---
title: "amc.analysis Description"
author: "Natali Stigler Leguizamo"
date: "2025-10-05"
output:
  html_document:
    keep_md: yes
---



#

'amc.analysis' is an R pacakge dedicated to...

## Usage

This is the original data:


``` r
library(amc.analysis)
data(mexico_shps)
```

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
##  6 1995_04008 2015_040… 1995… 2015…     1.07e9 1.07e 9 1.07e 9             100. 
##  7 1995_04003 2015_040… 1995… 2015…     7.11e9 1.28e10 7.11e 9              55.7
##  8 1995_04007 2015_040… 1995… 2015…     2.16e9 2.16e 9 2.16e 9             100. 
##  9 1995_04009 2015_040… 1995… 2015…     4.77e9 4.77e 9 4.77e 9             100  
## 10 1995_04004 2015_040… 1995… 2015…     8.62e9 1.55e10 1.40e10              55.8
## # ℹ 16 more rows
## # ℹ 3 more variables: area_b_overlapped <dbl>, year_A <dbl>, year_B <dbl>
```


