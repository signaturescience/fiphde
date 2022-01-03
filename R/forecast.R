#' @title Fit and forecast with time-series approaches.
#' @description Fit and forecast with time-series approaches.
#' @param prepped_hosp_tsibble A tsibble with data retrieved from [get_hdgov_hosp], prepped by [prep_hdgov_hosp], and made into a tsibble with [make_tsibble].
#' @param outcome The outcome variable to model (default `"flu.admits"`).
#' @param horizon Number of weeks ahead
#' @param trim_date The date (YYYY-MM-DD) at which point ts modeling should be started. Default `"2021-01-01"`. Set to `NULL` to stop trimming.
#' @param constrained Should the model be constrained to a non-seasonal model? If `TRUE` the parameter space defined in "param_space" argument will be used. See [fable::ARIMA].
#' @param param_space Named list for ARIMA parameter space constraint; only used if "constrained == `TRUE`"; default is `list(P=0,D=0,Q=0,p=1:2,d=0:2,0)`, which sets space to PDQ(0,0,0) and pdq(1:2,0:2,0).
#' @param covariates Covariates that should be modeled with the time series. Defaults to `c("hosp_rank", "ili_rank")`, from the historical data brought in with [prep_hdgov_hosp].
#' @param ensemble Should ARIMA and ETS models be ensembled? Default `TRUE`.
#' @return A list of the time series fit, time series forecast, and model formulas.
#' - `tsfit`: A `mdl_df` class "mable" with one row for each location, columns for arima and ets models.
#' - `tsfor`: A `fbl_ts` class "fable" with one row per location-model-timepoint up to `horizon` number of time points.
#' - `arima_formula`: A formula object: the ARIMA model formula used.
#' - `ets_formula`: A formula object: the nonseasonal exponential smoothing model formula used.
#' @export
#' @examples
#' \dontrun{
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' ## save(h_raw, file="~/Downloads/h_raw.rd")
#' ## load(file="~/Downloads/h_raw.rd")
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' prepped_hosp_tsibble <-
#'   prepped_hosp_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#' hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                constrained=TRUE,
#'                                covariates=c("hosp_rank", "ili_rank"))
#' }
ts_fit_forecast <- function(prepped_hosp_tsibble,
                            outcome="flu.admits",
                            horizon=4L,
                            trim_date="2021-01-01",
                            constrained=TRUE,
                            param_space=list(P=0,D=0,Q=0,p=1:2,d=0:2,q=0),
                            covariates=c("hosp_rank", "ili_rank"),
                            ensemble=TRUE) {

  if (!is.null(trim_date)) {
    message(sprintf("Trimming to %s", trim_date))
    prepped_hosp_tsibble <-
      prepped_hosp_tsibble %>%
      dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d"))
  }


  param_space <- lapply(param_space, deparse)
  if (constrained) {
    .stepwise <- FALSE
    .approximation <- FALSE
    PDQ <- sprintf("PDQ(%s,%s,%s)", param_space$P,param_space$D,param_space$Q)
    pdq <- sprintf("pdq(%s,%s,%s)", param_space$p,param_space$d,param_space$q)
    arima_formula <- stats::reformulate(c(PDQ, pdq, covariates), response=outcome)
  } else {
    .stepwise <- TRUE
    .approximation <- TRUE
    if (!is.null(covariates)) {
      arima_formula <- stats::reformulate(covariates, response=outcome)
    } else {
      arima_formula <- stats::reformulate("0", response=outcome)
    }
  }
  ets_formula <- stats::reformulate("season(method='N')", response=outcome)

  message(paste0("ARIMA formula: ", Reduce(paste, deparse(arima_formula))))
  message(paste0("ETS   formula: ", Reduce(paste, deparse(ets_formula))))

  tsfit <- fabletools::model(.data = prepped_hosp_tsibble,
                             arima = fable::ARIMA(arima_formula, stepwise=.stepwise, approximation=.stepwise),
                             ets = fable::ETS(ets_formula))

  # Ensemble the ARIMA and ETS models
  if (ensemble) {
    tsfit <-
      tsfit %>%
      dplyr::mutate(ensemble=(arima+ets)/2)
  }

  # forecast
  if (is.null(covariates)) {
    tsfor <- fabletools::forecast(tsfit, h=horizon)
  } else {
    # This is a known issue. In order to forecast with covariates supplied in arguments,
    # those covariates must be in the supplied tsibble, but you also need to figure out
    # how to get those covariates into the new data tsibble you're creating below.
    # This is easy with the historical severity, because it's by epiweek, not by epiweek/year,
    # So we can just join this back to the historical severity data. But if we have other
    # covariates, you'll have to figure out how to get them, via forecast or some other means.
    new_data <-
      tsibble::new_data(prepped_hosp_tsibble, n=horizon) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
      dplyr::inner_join(historical_severity, by="epiweek")
    tsfor <- fabletools::forecast(tsfit, new_data=new_data)
  }

  return(tibble::lst(tsfit, tsfor, arima_formula, ets_formula))

}

#' @title Forecast ILI
#' @description Forecasts ILI up to specified weeks in the future. Used in downstream modeling.
#' @details Currently limited to one location only.
#' @param ilidat Data returned from [get_cdc_ili].
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param trim_date Earliest start date you want to use for ILI data. Default `NULL` doesn't trim.
#' @param type Either "arima" or "ets" to fit a [fable::ARIMA] or [fable::ETS] model. If using "ets", the `constrained` and `param_space` arguments are ignored.
#' @param constrained Should the model be constrained to a non-seasonal model? If `TRUE` the parameter space defined in "param_space" argument will be used. See [fable::ARIMA].
#' @param param_space Named list for ARIMA parameter space constraint; only used if "constrained == `TRUE`"; default is `list(P=0,D=0,Q=0,p=1:2,d=0:2,0)`, which sets space to PDQ(0,0,0) and pdq(1:2,0:2,0).
#' @return A named list containing:
#' 1. `ilidat`: The data sent into the function filtered to the location and the `trim_date`. Select columns returned.
#' 1. `ilidat_tsibble`: The `tsibble` class object returned by running [make_tsibble] on the data above.
#' 1. `ili_fit`: The fit from [fabletools::model].
#' 1. `ili_forecast`: The forecast from [fabletools::forecast] at the specified horizon.
#' 1. `ili_future`: The `horizon`-number of weeks of ILI data forecasted into the future.
#' 1. `ili_bound`: The data in 1 bound to the data in 5.
#' 1. `arima_params`: A tibble with ARIMA model parameters for each location (if `type="arima"`).
#' 1. `locstats`: A tibble with missing data information on all locations.
#' 1. `removed`: A tibble with locations removed because of high missing ILI data.
#' @examples
#' \dontrun{
#' # Get data
#' ilidat <- get_cdc_ili(region = c("national", "state", "hhs"),
#'                       years = 2010:lubridate::year(lubridate::today()))
#'
#' # Using data only from march 2020 forward, for US only
#' ilidat_us <- ilidat %>% dplyr::filter(location=="US")
#' # Replace most recent week with nowcast data, and nowcast last week
#' ilidat_us <- ilidat_us %>% replace_ili_nowcast(weeks_to_replace=1)
#' ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01")
#' ilifor_us$ili_fit
#' ilifor_us$arima_params
#' ilifor_us$ili_forecast
#' head(ilifor_us$ili_bound)
#' tail(ilifor_us$ili_bound, 10)
#' # Plot
#' library(dplyr)
#' library(ggplot2)
#' theme_set(theme_classic())
#' ilifor_us$ili_bound %>%
#'   mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-03-01") %>%
#'   ggplot(aes(date, ili)) +
#'   geom_line(lwd=.3, alpha=.5) +
#'   geom_point(aes(col=forecasted), size=2)
#'
#' # At the state level
#' ilidat_st <- ilidat %>% dplyr::filter(region_type=="States")
#' ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2019-01-01", type="ets")
#' ilifor_st$ili_fit
#' ilifor_st$arima_params
#' ilifor_st$ili_forecast
#' head(ilifor_us$ili_bound)
#' tail(ilifor_us$ili_bound, 10)
#' # Plot
#' library(dplyr)
#' library(ggplot2)
#' theme_set(theme_classic())
#' ilifor_st$ili_bound %>%
#'   mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-08-01") %>%
#'   ggplot(aes(date, ili, col=forecasted)) +
#'   geom_line(lwd=.3) +
#'   geom_point(aes(col=forecasted), size=.7) +
#'   facet_wrap(~abbreviation, scale="free_y")
#'
#' ## At the HHS regional level
#' ilidat_hhs <- ilidat %>% dplyr::filter(region_type=="HHS Regions")
#' ilifor_hhs <- forecast_ili(ilidat_hhs, horizon=4L, trim_date="2020-03-01")
#' ilifor_hhs$ili_fit
#' ilifor_hhs$arima_params
#' ilifor_hhs$ili_forecast
#' head(ilifor_us$ili_bound)
#' tail(ilifor_us$ili_bound, 10)
#' # Plot
#' library(dplyr)
#' library(ggplot2)
#' theme_set(theme_classic())
#' ilifor_hhs$ili_bound %>%
#'   mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-08-01") %>%
#'   ggplot(aes(date, ili, col=forecasted)) +
#'   geom_line(lwd=.3) +
#'   geom_point(aes(col=forecasted), size=.7) +
#'   facet_wrap(~abbreviation, scale="free_y")
#'
#' ## hhs using exponential smoothing model
#' ilidat_hhs <- ilidat %>% dplyr::filter(region_type=="HHS Regions")
#' ilifor_hhs <- forecast_ili(ilidat_hhs, horizon=4L, type="ets", trim_date="2019-01-01")
#' ilifor_hhs$ili_bound %>%
#'   mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-08-01") %>%
#'   ggplot(aes(date, ili, col=forecasted)) +
#'   geom_line(lwd=.3) +
#'   geom_point(aes(col=forecasted), size=.7) +
#'   facet_wrap(~abbreviation, scale="free_y")
#' }
#' @export
forecast_ili <- function(ilidat, horizon=4L, trim_date=NULL, type="arima", constrained=TRUE, param_space = list(P=0,D=0,Q=0,p=1:2,d=0:2,q=0)) {

  # If trim_date is not null, trim to selected trim_date
  if (!is.null(trim_date)) {
    ilidat <-
      ilidat %>%
      dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d"))
  }

  # Select just the columns you care about, and call "ili" the measure you're using
  ilidat <-
    ilidat %>%
    dplyr::select(location, epiyear, epiweek, ili=weighted_ili)

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
    make_tsibble(epiyear = epiyear, epiweek = epiweek, key=location)

  if (type=="arima") {
    # Defaults to constrained, non-seasonal model.
    if (constrained) {
      # Nonseasonal fit: PDQ(0, 0, 0)
      # Nonseasonal components unrestricted: pdq(0:5,0:5,0:5)
      message("Fitting nonseasonal constrained ARIMA model...")
      ili_fit <- fabletools::model(ilidat_tsibble,
                                   arima = fable::ARIMA(ili ~ PDQ(param_space$P,param_space$D,param_space$Q) + pdq(param_space$p,param_space$d, param_space$q),
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

    # Get arima params if fitting an arima model
    arima_params <-
      ili_fit %>%
      dplyr::mutate(x=purrr::map(arima, ~purrr::pluck(., "fit") %>% purrr::pluck("spec"))) %>%
      tidyr::unnest_wider(col=x) %>%
      dplyr::select(-arima)

  } else if (type=="ets") {
    ili_fit <- fabletools::model(ilidat_tsibble, ets=fable::ETS(ili ~ season(method="N")))
    arima_params <- NULL
  } else {
    stop("type must be arima or ets")
  }


  # Get the forecast
  ili_forecast <- fabletools::forecast(ili_fit, h=horizon)

  # Get the next #horizon weeks in a tibble
  ili_future <- ili_forecast %>%
    tibble::as_tibble() %>%
    dplyr::mutate(epiyear=lubridate::epiyear(yweek)) %>%
    dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
    dplyr::select(location, epiyear, epiweek, ili=.mean)

  # bind the historical data to the new data
  ili_bound <- dplyr::bind_rows(ilidat %>% dplyr::mutate(forecasted=FALSE),
                                ili_future %>% dplyr::mutate(forecasted=TRUE)) %>%
    dplyr::arrange(location, epiyear, epiweek) %>%
    dplyr::inner_join(locations, by="location")

  # Create results
  res <- tibble::lst(ilidat, ilidat_tsibble, ili_fit, ili_forecast, ili_future, ili_bound, arima_params, locstats, removed)
  return(res)
}
