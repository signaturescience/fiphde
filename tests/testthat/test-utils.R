load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Utility functions", {
  expect_true(inherits(this_monday(), "Date"))
  expect_true(inherits(is_monday(), "logical"))
  expect_equal(mnz(c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)), .0001)
  expect_equal(mnz_replace(c(.1, 0, -.2, NA, .3, 42, .0001, -.3, NA)),
               c(0.1, 1e-04, 1e-04, NA, 0.3, 42, 1e-04, 1e-04, NA))
  expect_equal(mmwr_week_to_date(2020,53), as.Date("2020-12-27"))
  expect_equal(seq(1,100,7) %>% smoothie(n=4, weight=c(1,2,3,4)), 92)
  expect_equal(colnames(.mcga(tibble::tibble(`MyCol_1`=1, `_ThIs.or that`=2))), c("mycol_1", "this_or_that"))
  expect_equal(to_num(c("1,000", "<10", ">1e6", "42%", "1 2 3")),c(1000, 10, 1e+06, 42, 123))

  # test forecast plots
  # FIXME: this should probably actually use the forecasts that you produce, rather than the precomputed values.
  p <- plot_forecast(prepped_hosp, prepped_forecast_ts$ensemble)
  forcplot <- readRDS(system.file("testdata/forcplot.rds", package="fiphde", mustWork=TRUE))
  expect_true(inherits(p, "gg"))
  expect_equal(p$data, forcplot$data)
  # FIXME: This doesn't work
  # expect_equal(p, forcplot)
  # FIXME: This doesn't either. Can't unclass an environment. Also this function is deprecated.
  # expect_equal_to_reference(p, file=system.file("testdata/forcplot.rds", package="fiphde", mustWork=TRUE))

})

test_that("Nowcasting", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()
  skip_if_offline(host = "delphi.cmu.edu")
  # Check nowcast gets you data from 2020-01
  expect_true(inherits(get_nowcast_ili(epiyearweeks = "202201", dates=NULL), "tbl_df"))
  # Check nowcast currently returns missing data
  expect_true(is.na(suppressMessages(get_nowcast_ili(epiyearweeks = NULL))))
  # Check augmentation
  x <- suppressMessages(replace_ili_nowcast(ilidat, weeks_to_replace = 1))
  expect_true(inherits(x, "tbl_df"))
  expect_equal(nrow(x), nrow(ilidat)+2*length(unique(ilidat$location)))
})
