## load test data
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Time series modeling and forecasting works as expected", {

  set.seed(2023-01-30)
  hosp_fitfor_test <- ts_fit_forecast(prepped_tsibble, horizon=4L, outcome="flu.admits", covariates=TRUE)
  ## check that forecast values are equal
  expect_equal(hosp_fitfor$tsfor, hosp_fitfor_test$tsfor)

  ## if the outcome is mis-specified (in this case ili isnt what should be forecasted) then should error out
  expect_error({
    hosp_fitfor_test <- ts_fit_forecast(prepped_tsibble, horizon=4L, outcome="ili", covariates=TRUE)
  })

})
