## load test data
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Hospitalization data prep works as expected", {

  # prep_hdgov_hosp
  ## the prep_hdgov_hosp should create the national data from consituent states
  expect_false("US" %in% unique(hosp_raw$state))
  suppressMessages(prepped_hosp_test <- prep_hdgov_hosp(hosp_raw) %>% dplyr::filter(location %in% c("US", "36")))
  expect_true("US" %in% unique(prepped_hosp_test$location))
  # Check column expectations
  expected_columns <- c("abbreviation", "location", "week_start", "monday", "week_end",
                        "epiyear", "epiweek", "flu.admits", "flu.admits.cov", "ili_mean",
                        "ili_rank", "hosp_mean", "hosp_rank")
  expect_true(all(expected_columns %in% colnames(prepped_hosp_test)))

  ## generally expect that the prepped hosp object exactly matches
  expect_equal(prepped_hosp, prepped_hosp_test)

  ## check that trimming older data works
  ## should "trim" so that the earliest data is the epiyear/epiweek provided
  prepped_hosp_test2 <- prep_hdgov_hosp(hosp_raw, trim = list(epiyear = 2022, epiweek = 5))
  expect_true(min(prepped_hosp_test2$week_start) == "2022-01-30")
})


test_that("Time series prep returns expected classes and keys", {

  # make_tsibble
  ## use prepped_hosp loaded from testdata.rd
  prepped_tsibble_test <- make_tsibble(prepped_hosp,
                                       epiyear = epiyear,
                                       epiweek=epiweek,
                                       key=location)
  expect_true(inherits(prepped_tsibble_test, "tbl_ts"))
  expect_true(inherits(prepped_tsibble_test, "tbl_df"))
  expect_true(inherits(prepped_tsibble_test$yweek, "yearweek"))
  # Check that it's keyed by location and indexed by yweek
  expect_equal(as.character(tsibble::key(prepped_tsibble_test)[[1]]), "location")
  expect_equal(as.character(tsibble::index(prepped_tsibble_test)), "yweek")
  ## after tsibble prep make sure the returned object equals the original
  expect_equal(prepped_tsibble, prepped_tsibble_test)
})

