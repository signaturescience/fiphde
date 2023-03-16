#' @title Fit glm models
#'
#' @description
#'
#' This helper function is used in [glm_wrap] to fit a list of models and select the best one. The model selection procedure will use the root mean square error (RMSE) metric implemented in [yardstick::rmse] to select the best model.
#'
#' @param .data Data including all explanatory and outcome variables needed for modeling; must include column for "location"
#' @param .models List of models defined as [trending::trending_model] objects
#'
#' @return A `tibble` containing characteristics from the "best" `glm` model including:
#'
#'- **model_class**: The "type" of model for the best fit
#'- **fit**: The fitted model object for the best fit
#'- **location**: The geographic unit being modeled
#'- **data**: Original model fit data as a `tibble` in a list column
#'
glm_fit <- function(.data,
                    .models) {

  dat <- .data

  message(paste0("Location ... "))
  message(unique(dat$location))

  ## evaluate models and use metrics provided in arg
  res <-
    trendeval::evaluate_models(
      .models,
      dat,
      method = trendeval::evaluate_resampling,
      metrics = list(yardstick::rmse)
    )

  ## pull the best model by rmse
  best_by_rmse <-
    res %>%
    # dplyr::filter(purrr::map_lgl(warning, is.null)) %>%  # remove models that gave warnings
    dplyr::filter(purrr::map_lgl(error, is.null))  %>%   # remove models that errored
    dplyr::slice_min(rmse) %>%
    dplyr::select(model) %>%
    purrr::pluck(1,1)

  ## fit the model
  tmp_fit <-
    best_by_rmse %>%
    trending::fit(dat)

  ## construct tibble with model type, actual fit, and the location
  ret <- dplyr::tibble(model_class = best_by_rmse$model_class,
                       fit = tmp_fit,
                       location = unique(dat$location),
                       data = tidyr::nest(dat, fit_data = dplyr::everything()))

  message("Selected model ...")
  message(as.character(ret$fit$fitted_model$family)[1])

  message("Variables ...")
  message(paste0(names(ret$fit$fitted_model$coefficients), collapse = " + "))
  return(ret)

}


#' @title Get quantiles from prediction intervals
#'
#' @description
#'
#' This helper function runs the [trending::predict.trending_model_fit] method on a fitted model at specified values of "alpha" in order to create a range of prediction intervals. The processing also includes steps to convert the alpha to corresponding quantile values at upper and lower bounds. See "Details" for more information on the translation of "alpha" to quantile values. This function is used internally in [glm_forecast].
#'
#' @param fit Fitted model object from [glm_fit]
#' @param new_data A `tibble` with new data on which the [trending::predict.trending_model_fit] method should run
#' @param alpha Vector specifying the threshold(s) to be used for prediction intervals (PI); alpha of `0.05` would correspond to 95% PI; default is `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of intervals
#'
#' @details
#'
#' The "alpha" parameter defines the width of prediction interval (PI). For example, an alpha = 0.05 would correspond to a 95% PI. This function uses the PI(s) (per the alpha value(s) specified) to construct a range of quantiles that fall at lower and upper bound of each PI. Continuing from the example of alpha = 0.05, the quantile estimates returned would fall at 0.025 (lower bound of PI) and 0.975 (upper bound of PI).
#'
#' @return A `tibble` with forecasted data including the following columns:
#'
#' - **epiweek**: The epidemiological week for the forecasted horizon
#' - **epiyear**: The epidemiological year for the forecasted horizon
#' - **quantile**: The quantile for the forecasted value; `NA` for point estimate
#' - **value**: The forecasted value
#'
glm_quibble <- function(fit, new_data, alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2) {

  ## get the quantiles from the alpha
  q_lower <- alpha/2
  q_upper <- 1 - q_lower

  ## run the predict method on the fitted model
  ## use the given alpha
  fit %>%
    stats::predict(new_data, alpha = alpha, simulate_pi = FALSE, uncertain = TRUE) %>%
    ## get just the prediction interval bounds ...
    ## index (time column must be named index) ...
    dplyr::select(epiyear, epiweek, lower_pi, upper_pi) %>%
    ## reshape so that its in long format
    tidyr::gather(quantile, value, lower_pi:upper_pi) %>%
    ## and subout out lower_pi/upper_pi for the appropriate quantile
    dplyr::mutate(quantile = ifelse(quantile == "lower_pi", q_lower, q_upper))
}


#' @title Forecast glm models
#'
#' @description
#'
#' This function uses fitted model object from [glm_fit] and future covariate data to create probablistic forecasts at specific quantiles derived from the "alpha" parameter.
#'
#' @param .data Data including all explanatory and outcome variables needed for modeling
#' @param new_covariates Tibble with one column per covariate, and n rows for n horizons being forecasted
#' @param fit Fitted model object from \link[fiphde]{glm_fit}
#' @param alpha Vector specifying the threshold(s) to be used for prediction intervals; alpha of `0.05` would correspond to 95% PI; default is `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of intervals
#'
#' @return A `tibble` with forecasted data including the following columns:
#'
#' - **epiweek**: The epidemiological week for the forecasted horizon
#' - **epiyear**: The epidemiological year for the forecasted horizon
#' - **quantile**: The quantile for the forecasted value; `NA` for point estimate
#' - **value**: The forecasted value
#'
glm_forecast <- function(.data, new_covariates = NULL, fit, alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2) {

  ## get the last date from the data provided
  last_date <-
    .data %>%
    dplyr::arrange(location, epiweek, epiyear) %>%
    dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    dplyr::arrange(date) %>%
    dplyr::pull(date) %>%
    utils::tail(1)

  tmp <- .data
  ## set up "new data" with the epiweek/epiyear week being forecasted  ...
  ## and calculation of lagged flu admissions (1 to 4 weeks back)
  new_data <-
    dplyr::tibble(lag_1 = utils::tail(tmp$flu.admits, 4)[4],
                  lag_2 = utils::tail(tmp$flu.admits, 4)[3],
                  lag_3 = utils::tail(tmp$flu.admits, 4)[2],
                  lag_4 = utils::tail(tmp$flu.admits, 4)[1],
                  epiweek = lubridate::epiweek(last_date + 7),
                  epiyear = lubridate::epiyear(last_date + 7))

  ## this should allow for a constant
  if(!is.null(new_covariates)) {
    new_data <-
      cbind(new_data,new_covariates)
  }
  # ## take the fit object provided and use predict
  point_estimates <-
    fit %>%
    stats::predict(new_data, simulate_pi = FALSE) %>%
    dplyr::select(epiweek, epiyear, estimate) %>%
    dplyr::mutate(estimate = round(estimate)) %>%
    dplyr::mutate(quantile = NA) %>%
    dplyr::select(epiweek, epiyear, quantile, value = estimate)

  ## map the quibble function over the alphas
  quants <- purrr::map_df(alpha, .f = function(x) glm_quibble(fit = fit, new_data = new_data, alpha = x))

  ## prep data
  dplyr::bind_rows(point_estimates,quants) %>%
    dplyr::arrange(epiyear,epiweek, quantile) %>%
    dplyr::left_join(new_data, by = c("epiweek","epiyear")) %>%
    dplyr::select(epiweek,epiyear,quantile,value)
}

#' @title Run glm modeling and forecasting
#'
#' @description
#'
#' This is a wrapper function that pipelines influenza hospitalization modeling ([glm_fit]) and forecasting ([glm_forecast]).
#'
#' @param .data Data including all explanatory and outcome variables needed for modeling
#' @param .models List of models defined as [trending::trending_model] objects
#' @param new_covariates A `tibble` with one column per covariate, and n rows for n horizons being forecasted
#' @param horizon Number of weeks ahead for forecasting
#' @param alpha Vector specifying the threshold(s) to be used for prediction intervals (PI); alpha of `0.05` would correspond to 95% PI; default is `c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2` to create a range of intervals
#'
#' @return Named list with two elements:
#'
#' - **model**: Output from [glm_fit] with selected model fit
#' - **forecasts**: Output from [glm_forecast] with forecasts from each horizon combined as a single tibble
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve data to be used in fitting models
#' hosp_va <-
#'  get_hdgov_hosp(limitcols=TRUE) %>%
#'  prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
#'  dplyr::filter(abbreviation == "VA")
#'
#' # Define list of models
#' models <-
#'  list(
#'    poisson = trending::glm_model(flu.admits ~ hosp_rank + ili_rank, family = "poisson"),
#'    quasipoisson = trending::glm_model(flu.admits ~ hosp_rank + ili_rank, family = "quasipoisson"),
#'    negbin = trending::glm_nb_model(flu.admits ~ hosp_rank + ili_rank)
#'  )
#'
#' # Create new covariate data to feed into forecast procedure
#' new_cov <-
#'   dplyr::tibble(
#'     date = max(hosp_va$week_start) + c(7,14,21,28),
#'     epiweek = lubridate::epiweek(date),
#'     epiyear = lubridate::epiyear(date)
#'   ) %>%
#'   dplyr::left_join(
#'     fiphde:::historical_severity, by="epiweek"
#'   ) %>%
#'   dplyr::select(-epiweek,-epiyear)
#'
#' # Run the glm wrapper to fit and forecast
#' va_glm_res <- glm_wrap(.data = hosp_va, .models = models, new_covariates = new_cov, horizon = 4)
#' va_glm_res
#'
#' }
glm_wrap <- function(.data, .models, new_covariates = NULL, horizon = 4, alpha = c(0.01, 0.025, seq(0.05, 0.45, by = 0.05)) * 2) {

  tmp_fit <- glm_fit(.data, .models = .models)

  stopifnot(nrow(new_covariates) == horizon)
  message("Forecasting 1 week ahead")
  tmp_forc <-
    glm_forecast(.data = .data, new_covariates = new_covariates[1,], fit = tmp_fit$fit, alpha = alpha)

  forc_list <- list()
  forc_list[[1]] <- tmp_forc

  if(horizon > 1) {

    for(i in 2:horizon) {

      message(sprintf("Forecasting %d week ahead",i))

      prev_weeks <-
        do.call("rbind", forc_list)  %>%
        ## get point estimate ... which will have NA quantile value
        dplyr::filter(is.na(quantile)) %>%
        dplyr::rename(flu.admits = value) %>%
        dplyr::select(-quantile) %>%
        dplyr::mutate(flu.admits.cov = NA, location = "US")

      forc_list[[i]] <- glm_forecast(.data = dplyr::bind_rows(.data, prev_weeks),
                                     new_covariates = new_covariates[i,],
                                     fit = tmp_fit$fit,
                                     alpha = alpha)

    }
  }
  forc_res <- do.call("rbind", forc_list)
  return(list(model = tmp_fit, forecasts = forc_res))
}
