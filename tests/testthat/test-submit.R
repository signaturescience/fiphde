## load test data
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Forecast submission prep and validation works as expected", {

  set.seed(2023-01-30)
  hosp_fitfor_test <- ts_fit_forecast(prepped_tsibble, horizon=4L, outcome="flu.admits", covariates=TRUE)

  formatted_list <- format_for_submission(hosp_fitfor_test$tsfor, method = "ts")
  ens_forc <- formatted_list$ensemble
  ## NOTE: need to force the forecast date to the monday prior to the first horizon target end date
  ## find the minimum date (Saturday of horizon=1) and subtract 5 to get the correct forecast date
  ## necessary for forecast to validate properly
  ens_forc$forecast_date <- min(ens_forc$target_end_date) - 5

  ## check that the point quantiles aare
  point_quants <-
    ens_forc %>%
    dplyr::filter(type == "point") %>%
    dplyr::pull(quantile)

  expect_true(all(is.na(point_quants)))

  ## check that forecast validation works
  expect_true(validate_forecast(ens_forc)$valid)

})

