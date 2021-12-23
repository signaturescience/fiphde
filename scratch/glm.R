library(tidyverse)
library(trendeval)
library(trending)

glm_fit <- function(.data,
                    .models,
                    metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae)) {

  dat <- .data

  message(paste0("Location ... "))
  message(unique(dat$location))

  ## evaluate models and use metrics provided in arg
  res <-
    trendeval::evaluate_models(
    .models,
    dat,
    method = evaluate_resampling,
    metrics = metrics
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
    fit(dat)

  ## construct tibble with model type, actual fit, and the location
  ret <- dplyr::tibble(model_class = best_by_rmse$model_class,
                       fit = tmp_fit,
                       location = unique(dat$location),
                       data = tidyr::nest(dat, fit_data = everything()))

  message("Selected model ...")
  message(as.character(ret$fit$fitted_model$family)[1])

  message("Variables ...")
  message(paste0(names(ret$fit$fitted_model$coefficients), collapse = " + "))
  return(ret)

}



## helper to get the quantiles from prediction intervals
glm_quibble <- function(fit, new_data, alpha) {

  ## get the quantiles from the alpha
  q_lower <- alpha/2
  q_upper <- 1 - q_lower

  ## run the predict method on the fitted model
  ## use the given alpha
  fit %>%
    predict(new_data, alpha = alpha) %>%
    ## get just the prediction interval bounds ...
    ## index (time column must be named index) ...
    select(epiyear, epiweek, lower_pi, upper_pi) %>%
    ## reshape so that its in long format
    gather(quantile, value, lower_pi:upper_pi) %>%
    ## and subout out lower_pi/upper_pi for the appropriate quantile
    mutate(quantile = ifelse(quantile == "lower_pi", q_lower, q_upper))
}


## a generic forecast function to use fit objects from above
glm_forecast <- function(.data, new_covariates = NULL, fit, alpha = 0.05) {

  ## get the last date from the data provided
  last_date <-
    .data %>%
    dplyr::arrange(location, epiweek, epiyear) %>%
    dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    dplyr::pull(date) %>%
    tail(1)

  tmp <- .data
  new_data <-
    dplyr::tibble(lag_1 = tail(tmp$flu.admits, 4)[4],
                  lag_2 = tail(tmp$flu.admits, 4)[3],
                  lag_3 = tail(tmp$flu.admits, 4)[2],
                  lag_4 = tail(tmp$flu.admits, 4)[1],
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
    predict(new_data) %>%
    dplyr::select(epiweek, epiyear, estimate) %>%
    dplyr::mutate(estimate = round(estimate)) %>%
    dplyr::mutate(quantile = NA) %>%
    dplyr::select(epiweek, epiyear, quantile, value = estimate)

  ## map the quibble function over the alphas
  quants <- map_df(alpha, .f = function(x) glm_quibble(fit = fit, new_data = new_data, alpha = x))

  ## prep data
  dplyr::bind_rows(point_estimates,quants) %>%
    dplyr::arrange(epiyear,epiweek, quantile) %>%
    dplyr::left_join(new_data, by = c("epiweek","epiyear")) %>%
    dplyr::select(epiweek,epiyear,quantile,value)
}

glm_wrap <- function(.data, .models, new_covariates = NULL, metrics = list(yardstick::rmse, yardstick::huber_loss, yardstick::mae), horizon = 4, alpha = 0.05) {
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

      forc_list[[i]] <- glm_forecast(.data = bind_rows(.data, prev_weeks),
                                     new_covariates = new_covariates[i,],
                                     fit = tmp_fit$fit,
                                     alpha = alpha)

    }
  }
  forc_res <- do.call("rbind", forc_list)
  return(list(model = tmp_fit, forecasts = forc_res))
}

plot_forc <- function(.forecasts, .train, .test) {

  forc_dat <-
    .forecasts %>%
    dplyr::filter(quantile %in% c(NA,0.025,0.975)) %>%
    tidyr::spread(quantile,value) %>%
    dplyr::rename(lower = `0.025`, upper = `0.975`, mean = `<NA>`)

  .test %>%
    dplyr::bind_rows(.train) %>%
    dplyr::select(epiweek,epiyear, truth = flu.admits, location) %>%
    dplyr::left_join(forc_dat) %>%
    dplyr::mutate(date = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(date,truth), lwd = 2, col = "black") +
    ggplot2::geom_line(ggplot2::aes(date,mean), lwd = 2, alpha = 0.5, lty = "solid", col = "firebrick") +
    ggplot2::geom_ribbon(ggplot2::aes(date, ymin = lower, ymax = upper), alpha = 0.25, fill = "firebrick") +
    ## get an upper limit from whatever the max of observed or forcasted hospitalizations is
    ggplot2::scale_y_continuous(limits = c(0,max(c(.test$flu.admits, .train$flu.admits, forc_dat$upper)))) +
    ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "month") +
    ggplot2::labs(x = "Date", y = "Count", title = "Influenza hospitalizations") +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~ location)

}

wis_score <- function(.forecasts, .test) {
  .forecasts %>%
    dplyr::left_join(test_dat) %>%
    dplyr::select(epiweek,epiyear,quantile,value,flu.admits) %>%
    dplyr::group_by(epiweek, epiyear) %>%
    dplyr::summarise(wis = evalcast::weighted_interval_score(quantile = quantile, value = value, actual_value = flu.admits))
}
