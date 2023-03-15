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

test_that("Categorical forecasting operates as expected", {

  set.seed(2023-01-30)
  hosp_fitfor_test <- ts_fit_forecast(prepped_tsibble, horizon=4L, outcome="flu.admits", covariates=TRUE)
  formatted_list <- format_for_submission(hosp_fitfor_test$tsfor, method = "ts")
  ens_forc <- formatted_list$ensemble
  cat_forcs <- forecast_categorical(ens_forc, prepped_hosp)

  ## check that categorical forecasts include all type_id values
  expect_equal(sort(unique(as.character(cat_forcs$type_id))), c("decrease","increase","large_decrease","large_increase","stable"))

  ## check that categorical forecasts all type id values are represented in every location
  n_locs <- length(unique(ens_forc$location))
  type_id_counts <-
    cat_forcs %>%
    dplyr::count(type_id) %>%
    dplyr::pull(n)

  expect_true(all(type_id_counts == n_locs))

})
