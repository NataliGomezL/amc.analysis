library(amc.analysis)


data(mexico_shps)

test_that("amc_overlap_analysis", {
  expect_no_error((raw_overlap <- amc_overlap_analysis(shps =  mexico_shps,
                                                       id_unit_var = "cvegeo",
                                                       period_var = "year")))
})

