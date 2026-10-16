## load test data
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
test_that("Utility functions work as expected", {
  expect_true(inherits(this_monday(), "Date"))
  expect_true(inherits(is_monday(), "logical"))
  expect_equal(mnz(c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)), .0001)
  expect_equal(mnz_replace(c(.1, 0, -.2, NA, .3, 42, .0001, -.3, NA)),
               c(0.1, 1e-04, 1e-04, NA, 0.3, 42, 1e-04, 1e-04, NA))
  expect_equal(mmwr_week_to_date(2020,53), as.Date("2020-12-27"))
  expect_equal(seq(1,100,7) %>% smoothie(n=4, weight=c(1,2,3,4)), 92)
  expect_equal(colnames(.mcga(tibble::tibble(`MyCol_1`=1, `_ThIs.or that`=2))), c("mycol_1", "this_or_that"))
  expect_equal(to_num(c("1,000", "<10", ">1e6", "42%", "1 2 3")),c(1000, 10, 1e+06, 42, 123))

})

test_that("Forecast plots return expected ggplot2 data", {

  # test forecast plots
  p <- plot_forecast(prepped_hosp, prepped_forecast_ts$ensemble)
  expect_true(inherits(p, "gg"))
  expect_equal(p$data, forcplot$data)
  ## testhat v3 uses waldo for comparison
  ## recommended to use vdiffr for ggplot2 tests
  ## https://github.com/r-lib/waldo/issues/56
  ## one workaround could be ...
  ## but this breaks out of the testthat recommendations
  ## expect_true(all.equal(p,forcplot))

})