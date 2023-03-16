## load test data
## NOTE: this loads hosp_fitfor, hosp_raw, ilidat, ilifor, prepped_forecast_ts, prepped_hosp, prepped_tsibble
## testdata.rd created with inst/testdata/testdata-create.R
load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))


test_that("The glm models fit and quibble works as expected", {

  ## fit the glm model using prepped_hosp loaded above
  models <-
    list(
      quasipoisson = trending::glm_model(flu.admits ~ hosp_rank + ili_rank, family = "quasipoisson")
    )

  fit_tst <- fiphde:::glm_fit(.data = prepped_hosp, .models = models)

  ## test that the class of the fit column is what we would expect out of trending
  expect_s3_class(fit_tst$fit, c("trending_model_fit","list"))


  ## define new cov
  new_cov <-
    dplyr::tibble(
      date = max(prepped_hosp$week_start) + c(7,14,21,28),
      epiweek = lubridate::epiweek(date),
      epiyear = lubridate::epiyear(date)
    ) %>%
    dplyr::left_join(
      fiphde:::historical_severity, by="epiweek"
    )

  ## test that the quibble returns expected quantiles
  quibble_tst <- fiphde:::glm_quibble(fit = fit_tst$fit, new_data = new_cov, alpha = 0.05)
  q_tst <- unique(quibble_tst$quantile)
  expect_equal(q_tst, c(0.025,0.975))

  quibble_tst2 <- fiphde:::glm_quibble(fit = fit_tst$fit, new_data = new_cov, alpha = 0.4)
  q_tst2 <- unique(quibble_tst2$quantile)
  expect_equal(q_tst2, c(0.2,0.8))

})

