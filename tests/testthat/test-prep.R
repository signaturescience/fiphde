load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Time series prep", {

  # prep_hdgov_hosp
  suppressMessages(prepped_hosp <- prep_hdgov_hosp(hosp_raw))
  expect_true(inherits(prepped_tsibble, "tbl_df"))
  # Make sure all the states are represented, except for US, which is added
  expect_true("US" %in% prepped_hosp$abbreviation)
  expect_true(all(grep("US", prepped_hosp$abbreviation, invert = TRUE, value = TRUE) %in% hosp_raw$state))
  # Check column expectations
  expected_columns <- c("abbreviation", "location", "week_start", "monday", "week_end",
                        "epiyear", "epiweek", "flu.admits", "flu.admits.cov", "ili_mean",
                        "ili_rank", "hosp_mean", "hosp_rank")
  expect_true(all(expected_columns %in% colnames(prepped_hosp)))

  # make_tsibble
  prepped_tsibble <- make_tsibble(prepped_hosp,
                                  epiyear = epiyear,
                                  epiweek=epiweek,
                                  key=location)
  expect_true(inherits(prepped_tsibble, "tbl_ts"))
  expect_true(inherits(prepped_tsibble, "tbl_df"))
  expect_true(inherits(prepped_tsibble$yweek, "yearweek"))
  # Check that it's keyed by location and indexed by yweek
  expect_equal(as.character(tsibble::key(prepped_tsibble)[[1]]), "location")
  expect_equal(as.character(tsibble::index(prepped_tsibble)), "yweek")

})
