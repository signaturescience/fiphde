#' @title Forecast ILI
#' @description Forecasts ILI up to specified weeks in the future. Used in downstream modeling.
#' @details Currently limited to one location only.
#' @param ilidat Data returned from [get_cdc_ili].
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param trim_date Earliest start date you want to use for ILI data. Default `NULL` doesn't trim.
#' @param constrained Should the model be constrained to a non-seasonal model? Default `TRUE` sets PQD(0,0,0) & pdq(0:5,0:5,0:5). See [fable::ARIMA].
#' @return A named list containing:
#' 1. `ilidat`: The data sent into the function filtered to the location and the `trim_date`. Select columns returned.
#' 1. `ilidat_tsibble`: The `tsibble` class object returned by running [make_tsibble] on the data above.
#' 1. `ili_fit`: The fit from [fabletools::model].
#' 1. `ili_forecast`: The forecast from [fabletools::forecast] at the specified horizon.
#' 1. `ili_future`: The `horizon`-number of weeks of ILI data forecasted into the future.
#' 1. `ili_bound`: The data in 1 bound to the data in 5.
#' 1. `arima_params`: A tibble with ARIMA model parameters for each location.
#' 1. `locstats`: A tibble with missing data information on all locations.
#' 1. `removed`: A tibble with locations removed because of high missing ILI data.
#' @examples
#' \dontrun{
#' # Get data
#' ilidat <- get_cdc_ili(region=c("national", "state"), years=2010:lubridate::year(lubridate::today()))
#'
#' # Using data only from march 2020 forward, for US only
#' ilidat_us <- ilidat %>% dplyr::filter(location=="US")
#' ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01")
#' head(ilifor_us$ili_bound)
#' tail(ilifor_us$ili_bound, 10)
#' ilifor_us$ili_fit
#' ilifor_us$ili_fit %>% focustools::extract_arima_params()
#' ilifor_us$arima_params
#' ilifor_us$ili_forecast
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
#' inner_join(ilifor_2020$ili_bound %>% rename(nonseasonal_unweighted_ili=unweighted_ili),
#'            ilifor_2010$ili_bound %>% rename(unconstrained_unweighted_ili=unweighted_ili),
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
forecast_ili <- function(ilidat, horizon=4L, trim_date=NULL, constrained=TRUE) {

  # If trim_date is not null, trim to selected trim_date
  if (!is.null(trim_date)) {
    ilidat <-
      ilidat %>%
      dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d"))
  }

  # Select just the columns you care about, and call "ili" the measure you're using
  ilidat <-
    ilidat %>%
    dplyr::select(location, year, week, ili=unweighted_ili)

  # Get missing data rates
  locstats <- ilidat %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(miss=sum(is.na(ili)), total=dplyr::n()) %>%
    dplyr::mutate(pmiss=miss/total) %>%
    dplyr::arrange(dplyr::desc(pmiss)) %>%
    dplyr::mutate(remove=pmiss>.1)

  # Get locations that will be removed
  removed <- locstats %>%
    dplyr::filter(remove) %>%
    dplyr::inner_join(locations, by="location")
  if(nrow(removed)>0) message(sprintf("Removed %s row(s) because of missing data. See result$removed.", nrow(removed)))

  # Remove those locations
  ilidat <- locstats %>%
    dplyr::filter(!remove) %>%
    dplyr::distinct(location) %>%
    dplyr::inner_join(ilidat, by="location")

  ## make a tsibble. do not chop the last week - because this is weekly data we won't have an incomplete final week
  ilidat_tsibble <-
    ilidat %>%
    make_tsibble(epiyear = year, epiweek = week, key=location, chop=FALSE)

  # Defaults to constrained, non-seasonal model.
  if (constrained) {
    # Nonseasonal fit: PDQ(0, 0, 0)
    # Nonseasonal components unrestricted: pdq(0:5,0:5,0:5)
    message("Fitting nonseasonal ARIMA model ~ PDQ(0,0,0) + pdq(0:5,0:5,0:5)")
    ili_fit <- fabletools::model(ilidat_tsibble,
                                 arima = fable::ARIMA(ili ~ PDQ(0,0,0) + pdq(0:5,0:5,0:5),
                                                      stepwise=FALSE,
                                                      approximation=FALSE))
  } else {
    # If unconstrained, need to set stepwise=TRUE and approxmiation=NULL to speed up.
    message("Fitting unconstrained ARIMA model...")
    ili_fit <- fabletools::model(ilidat_tsibble,
                                 arima = fable::ARIMA(ili,
                                                      stepwise=TRUE,
                                                      approximation=NULL))
  }


  # arima_params <- unlist(ili_fit$arima[[1]]$fit$spec[,1:6])
  arima_params <-
    ili_fit %>%
    dplyr::mutate(x=purrr::map(arima, ~purrr::pluck(., "fit") %>% purrr::pluck("spec"))) %>%
    tidyr::unnest_wider(col=x) %>%
    dplyr::select(-arima)


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
    dplyr::select(location, year, week, ili=.mean)

  # bind the historical data to the new data
  ili_bound <- dplyr::bind_rows(ilidat     %>% dplyr::mutate(forecasted=FALSE),
                                ili_future %>% dplyr::mutate(forecasted=TRUE)) %>%
    dplyr::arrange(location, year, week)

  # Create results
  res <- tibble::lst(ilidat, ilidat_tsibble, ili_fit, ili_forecast, ili_future, ili_bound, arima_params, locstats, removed)
  return(res)
}
