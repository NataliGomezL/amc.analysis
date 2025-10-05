library(sf)
suppressPackageStartupMessages(library(dplyr))

M1 = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
M2 <- M1+0.01
M3 = matrix(c(1,1,1,4,4,4,4,1,1,1),ncol=2, byrow=TRUE)
M4 <- M3-5
M5 <- M3-6
M6 <- M3-7
M7 = matrix(c(11,11,11,12,12,12,12,11,11,11),ncol=2, byrow=TRUE)
M8 = matrix(c(0,-2,4,-2,4,-4,0,-4,0,-2),ncol=2, byrow=TRUE)
M9 <- M8+1

M_all <- list(M1, M2, M3, M4, M5, M6, M7, M8, M9)
pl2 = purrr::map(M_all, ~st_polygon(list(.)))
FC <- st_sf(ids = LETTERS[1:length(pl2)], geometry=st_sfc(pl2), x=rnorm(length(pl2)))


test_that("overlap works", {
  expect_no_error(amc.analysis:::ovr_get_overlap_pairs(sf=FC) %>%
                    amc.analysis:::ovr_add_group()
  )
})
