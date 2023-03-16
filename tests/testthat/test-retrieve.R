## load test data
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("HHS data retrieval works", {

  ## pull the hospitalization data from hhs
  hosp_data_tst <- get_hdgov_hosp()

  ## check that the object returned is a tibble
  expect_s3_class(hosp_data_tst, "tbl_df")

  ## check that the tibble has contents
  expect_true(nrow(hosp_data_tst) > 0)

})
