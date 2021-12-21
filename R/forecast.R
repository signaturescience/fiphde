#' @title Forecast ILI
#' @description Forecasts ILI up to specified weeks in the future. Used in downstream modeling.
#' @details Currently limited to one location only.
#' @param ilidat Data returned from [get_cdc_ili].
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param location Vector specifying locations to filter to; `'US'` by default.
#' @param trim_date Earliest start date you want to use for ILI data. Default `NULL` doesn't trim.
#' @param constrained Should the model be constrained to a non-seasonal model? Default `TRUE` sets PQD(0,0,0) & pdq(0:5,0:5,0:5). See [fable::ARIMA].
#' @return A named list containing:
#' 1. `ilidat`: The data sent into the function filtered to the location and the `trim_date`. Select columns returned.
#' 1. `ilidat_tsibble`: The `tsibble` class object returned by running [make_tsibble] on the data above.
#' 1. `ili_fit`: The fit from [fabletools::model].
#' 1. `ili_forecast`: The forecast from [fabletools::forecast] at the specified horizon.
#' 1. `ili_future`: The `horizon`-number of weeks of ILI data forecasted into the future.
#' 1. `ili_bound`: The data in 1 bound to the data in 5.
#' 1. `arima_params`: A named character vector of the ARIMA model parameters.
#' @examples
#' \dontrun{
#' # Get data
#' ilidat <- get_cdc_ili(region="national", years=2010:lubridate::year(lubridate::today()))
#' # Using data only from march 2020 forward
#' ilifor_2020 <- forecast_ili(ilidat, horizon=4L, location="US", trim_date="2020-03-01")
#' head(ilifor_2020$ili_bound)
#' tail(ilifor_2020$ili_bound, 10)
#' ilifor_2020$ili_fit
#' ilifor_2020$ili_fit %>% focustools::extract_arima_params()
#' ilifor_2020$arima_params
#' ilifor_2020$ili_forecast
#' # Using all the data we have (2010-forward, in this example)
#' ilifor_2010 <- forecast_ili(ilidat, horizon=4L, location="US", constrained=FALSE)
#' head(ilifor_2010$ili_bound)
#' tail(ilifor_2010$ili_bound, 10)
#' ilifor_2010$ili_fit
#' ilifor_2010$ili_fit %>% focustools::extract_arima_params()
#' ilifor_2010$arima_params
#' ilifor_2010$ili_forecast
#' # Plot both forecasts
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' inner_join(ilifor_2020$ili_bound %>% rename(nonseasonal_weighted_ili=weighted_ili),
#'            ilifor_2010$ili_bound %>% rename(unconstrained_weighted_ili=weighted_ili),
#'            by = c("location", "year", "week", "forecasted")) %>%
#'   gather(key, value, ends_with("ili")) %>%
#'   mutate(date=cdcfluview::mmwr_week_to_date(year, week)) %>%
#'   filter(date>"2021-07-01") %>%
#'   ggplot(aes(date, value)) +
#'   geom_line(alpha=.5) +
#'   geom_point(aes(col=forecasted)) +
#'   facet_wrap(~key) +
#'   theme_bw()
#' }
#' @export
forecast_ili <- function(ilidat, horizon=4L, location="US", trim_date=NULL, constrained=TRUE) {

  # If trim_date is not null, trim to selected trim_date
  if (!is.null(trim_date)) {
    ilidat <-
      ilidat %>%
      dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d"))
  }

  ## subset to selected location and get columns you care about
  ilidat <-
    ilidat %>%
    dplyr::filter(location %in% location) %>%
    dplyr::select(location, year, week, weighted_ili)


  ## make a tsibble. do not chop the last week - because this is weekly data we won't have an incomplete final week
  ilidat_tsibble <-
    ilidat %>%
    fiphde::make_tsibble(epiyear = year, epiweek = week, chop=FALSE)

  # Defaults to constrained, non-seasonal model.
  if (constrained) {
    # Nonseasonal fit: PDQ(0, 0, 0)
    # Nonseasonal components unrestricted: pdq(0:5,0:5,0:5)
    message("Fitting nonseasonal ARIMA model ~ PDQ(0,0,0) + pdq(0:5,0:5,0:5)")
    ili_fit <- fabletools::model(ilidat_tsibble,
                                 arima = fable::ARIMA(weighted_ili ~ PDQ(0,0,0) + pdq(0:5,0:5,0:5),
                                                      stepwise=FALSE,
                                                      approximation=FALSE))
  } else {
    # If unconstrained, need to set stepwise=TRUE and approxmiation=NULL to speed up.
    message("Fitting unconstrained ARIMA model...")
    ili_fit <- fabletools::model(ilidat_tsibble,
                                 arima = fable::ARIMA(weighted_ili,
                                                      stepwise=TRUE,
                                                      approximation=NULL))
  }

  arima_params <- unlist(ili_fit$arima[[1]]$fit$spec[,1:6])

  # Get the forecast
  ili_forecast <- fabletools::forecast(ili_fit, h=horizon)

  ## Look at the quantiles
  # ili_forecast %>%
  #   fabletools::hilo()
  # ili_forecast %>%
  #   fabletools::hilo() %>%
  #   fabletools::unpack_hilo(`80%`) %>%
  #   fabletools::unpack_hilo(`95%`)

  # Get the next #horizon weeks in a tibble
  ili_future <- ili_forecast %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year=lubridate::epiyear(yweek)) %>%
    dplyr::mutate(week=lubridate::epiweek(yweek)) %>%
    dplyr::select(location, year, week, weighted_ili=.mean)

  # bind the historical data to the new data
  ili_bound <- dplyr::bind_rows(ilidat     %>% dplyr::mutate(forecasted=FALSE),
                                ili_future %>% dplyr::mutate(forecasted=TRUE))

  # Create results
  res <- tibble::lst(ilidat, ilidat_tsibble, ili_fit, ili_forecast, ili_future, ili_bound, arima_params)
  return(res)
}
