#' @title Fit and forecast with time-series approaches.
#' @description
#'
#' This function allows the user to fit time series models and forecast values out to a specified horizon. Starting from a `tsibble` object (see [make_tsibble]), the function fits the models specified as a list in the "models" argument. The "Details" section provides more information on how to parameterize the models used. Note that if the input `tsibble` is "keyed" (e.g., grouped by location) then the procedure will fit and forecast independently for each grouping.
#'
#' @param prepped_tsibble A `tsibble` with data formatted via [make_tsibble]
#' @param outcome The outcome variable to model; default is `"flu.admits"`
#' @param horizon Number of weeks ahead to forecast
#' @param trim_date The date (YYYY-MM-DD) at which time series models should start fitting; default `"2021-01-01"`; if set to `NULL` the input data will not be trimmed (i.e., all data will be used to fit time series models)
#' @param models A list of right hand side formula contents for models you want to run; default is `list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)', ets='season(method="N")', nnetar=NULL)` which runs a constrained ARIMA, non-seasonal ETS, and ignores the NNETAR model; see "Details" for more information
#' @param covariates Covariates that should be modeled with the time series. Defaults to `c("hosp_rank", "ili_rank")`, from the historical data brought in with [prep_hdgov_hosp].
#' @param ensemble Logical as to whether or not the models should be ensembled (using mean); default `TRUE`
#' @param remove_null_models Logical as to whether or null models should be removed; default `TRUE`
#' @return A list of the time series fit, time series forecast, and model formulas.
#' - **tsfit**: A `mdl_df` class "mable" with one row for each location, columns for arima and ets models.
#' - **tsfor**: A `fbl_ts` class "fable" with one row per location-model-timepoint up to `horizon` number of time points.
#' - **formulas**: A list of ARIMA, ETS, and/or NNETAR formulas
#'
#' @details
#'
#' When fitting time series models, the set of models used (and their parameters) can be defined via a named list passed to the "models" argument. The list should contain elements that define the right-hand side of model formulas. The function internally uses the [fable::fable] package, and any models provided must be part of the `fable` ecosystem of time series models. The models passed must be named as "arima", "ets", and "nnetar". To skip any one of these models set the named argument for the given model to `NULL`. The "models" argument defaults to `list(arima = "PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)", ets = "season(method='N')", nnetar = NULL)`. To run an unconstrained ARIMA: `list(arima='PDQ() + pdq()')` (see [fable::ARIMA]). To run a seasonal exponential smoothing: `list(ets='season(method=c("A", "M", "N"), period="3 months")')` (see [fable::ETS]). To run an autoregressive neural net with P=1: `list(nnetar="AR(P=1)")` (see [fable::NNETAR]).
#'
#' @references <https://fable.tidyverts.org/>
#' @export
#' @examples
#' \dontrun{
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' prepped_tsibble <- make_tsibble(prepped_hosp,
#'                                 epiyear = epiyear,
#'                                 epiweek=epiweek,
#'                                 key=location)
#' prepped_tsibble <-
#'   prepped_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#' # Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
#' hosp_fitfor1 <- ts_fit_forecast(prepped_tsibble,
#'                                 horizon=4L,
#'                                 outcome="flu.admits",
#'                                 covariates=c("hosp_rank", "ili_rank"))
#' # Run an unconstrained ARIMA, seasonal ETS, no NNETAR
#' hosp_fitfor2 <- ts_fit_forecast(prepped_tsibble,
#'                                 horizon=4L,
#'                                 outcome="flu.admits",
#'                                 covariates=c("hosp_rank", "ili_rank"),
#'                                 models=list(arima='PDQ() + pdq()',
#'                                             ets='season(method=c("A", "M", "N"), period="3 months")',
#'                                             nnetar=NULL))
#' # Run an unconstrained ARIMA, seasonal ETS, NNETAR
#' hosp_fitfor3 <- ts_fit_forecast(prepped_tsibble,
#'                                 horizon=4L,
#'                                 outcome="flu.admits",
#'                                 covariates=c("hosp_rank", "ili_rank"),
#'                                 models=list(arima='PDQ() + pdq()',
#'                                             ets='season(method=c("A", "M", "N"), period="3 months")',
#'                                             nnetar="AR(P=1)"))
#' }
ts_fit_forecast <- function(prepped_tsibble,
                            outcome="flu.admits",
                            horizon=4L,
                            trim_date="2021-01-01",
                            models=list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)',
                                        ets='season(method="N")',
                                        nnetar=NULL),
                            covariates=c("hosp_rank", "ili_rank"),
                            ensemble=TRUE,
                            remove_null_models=TRUE) {

  if (!is.null(trim_date)) {
    message(sprintf("Trimming to %s", trim_date))
    prepped_tsibble <-
      prepped_tsibble %>%
      dplyr::filter(week_start > as.Date(trim_date, format = "%Y-%m-%d"))
  }

  # create a list of model formulas
  formulas <- list()

  # Create a list to hold model objects. Each model has a location column and a column for that model
  tsfit <- list()

  # If "arima" is in the models you specify, fit an arima model
  if (!is.null(models$arima)) {
    formulas$arima <- stats::reformulate(c(models$arima, covariates), response=outcome)
    message(paste0("ARIMA  formula: ", Reduce(paste, deparse(formulas$arima))))
    tsfit$arima <- fabletools::model(.data = prepped_tsibble,
                                     arima = fable::ARIMA(formulas$arima))
  }

  # If "ets" is in the models you specify, fit an ETS model
  if (!is.null(models$ets)) {
    formulas$ets <- stats::reformulate(models$ets, response=outcome)
    message(paste0("ETS    formula: ", Reduce(paste, deparse(formulas$ets))))
    tsfit$ets   <- fabletools::model(.data = prepped_tsibble,
                                     ets = fable::ETS(formulas$ets))
  }

  if (!is.null(models$nnetar)) {
    formulas$nnetar <- stats::reformulate(models$nnetar, response=outcome)
    message(paste0("NNETAR formula: ", Reduce(paste, deparse(formulas$nnetar))))
    tsfit$nnetar <- fabletools::model(.data = prepped_tsibble,
                                      nnetar = fable::NNETAR(formulas$nnetar))
  }

  # equivalent to:
  # tsfit[[1]] %>% inner_join(tsfit[[2]]) %>% inner_join(tsfit[[3]]) %>% inner_join(tsfit[[4]])...
  # this may need to be a full_join, if some locations that don't fit don't come through at all.
  # e.g. if an ARIMA model doesn't fit for one location but the ETS does, you still want the ETS model for that location.
  tsfit <- purrr::reduce(tsfit, dplyr::inner_join, by="location")

  # Ensemble the ARIMA and ETS models
  # Hard-coded for now - can always ensemble other models if/when we add them.
  # fixme: this could be more flexible
  if (ensemble & !is.null(models$arima) & !is.null(models$ets)) {
    tsfit <-
      tsfit %>%
      dplyr::mutate(ensemble=(arima+ets)/2)
  }

  # Find which location/models are null models.
  # Can result in multiple rows for one location if multiple models are NULL
  nullmodels <-
    tsfit %>%
    tibble::as_tibble() %>%
    tidyr::gather(model, value, -location) %>%
    dplyr::mutate(is_null_model=fabletools::is_null_model(value)) %>%
    dplyr::filter(is_null_model) %>%
    dplyr::distinct(location, model)

  # Get rid of entire rows if one of the models is null
  tsfit <-
    nullmodels %>%
    dplyr::distinct(location) %>%
    dplyr::anti_join(tsfit, ., by="location")

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
      tsibble::new_data(prepped_tsibble, n=horizon) %>%
      dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
      dplyr::inner_join(historical_severity, by="epiweek")
    tsfor <- fabletools::forecast(tsfit, new_data=new_data)
  }

  return(tibble::lst(tsfit, tsfor, formulas, nullmodels))

}

#' @title Forecast ILI
#' @description This function forecasts ILI up to a specified future horizon. The models used can be parameterized with a "models" argument (for more details see [ts_fit_forecast]). By default, the function will use an ARIMA approach to model all locations in the input historical ILI data and then use the fitted models forecast out to each of the horizons.
#' @param ilidat Data returned from [get_cdc_ili]
#' @param horizon Optional horizon periods through which the forecasts should be generated; default is `4`
#' @param trim_date Earliest start date you want to use for ILI data; default `NULL` doesn't trim
#' @param models The list of model parameters passed to [ts_fit_forecast]; defaults to `list(arima="PDQ(0,0,0)+pdq(1:2,0:2,0)"`
#' @return A named list containing:
#' - **ilidat**: The data sent into the function filtered to the location and the `trim_date`. Select columns returned.
#' - **ilidat_tsibble**: The `tsibble` class object returned by running [make_tsibble] on the data above.
#' - **ili_fit**: The fit from [fabletools::model].
#' - **ili_forecast**: The forecast from [fabletools::forecast] at the specified horizon.
#' - **ili_future**: The `horizon`-number of weeks of ILI data forecasted into the future.
#' - **ili_bound**: The data in 1 bound to the data in 5.
#' - **arima_params**: A tibble with ARIMA model parameters for each location (if `type="arima"`).
#' - **locstats**: A tibble with missing data information on all locations.
#' - **removed**: A tibble with locations removed because of high missing ILI data.
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
#'   mutate(date=mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-03-01") %>%
#'   ggplot(aes(date, ili)) +
#'   geom_line(lwd=.3, alpha=.5) +
#'   geom_point(aes(col=forecasted), size=2)
#'
#' # At the state level
#' ilidat_st <- ilidat %>% dplyr::filter(region_type=="States")
#' ilifor_st <- forecast_ili(ilidat_st, horizon=4L, trim_date="2019-01-01",
#'                           models=list(ets="season(method='N')"))
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
#'   mutate(date=mmwr_week_to_date(epiyear, epiweek)) %>%
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
#'   mutate(date=mmwr_week_to_date(epiyear, epiweek)) %>%
#'   filter(date>"2021-08-01") %>%
#'   ggplot(aes(date, ili, col=forecasted)) +
#'   geom_line(lwd=.3) +
#'   geom_point(aes(col=forecasted), size=.7) +
#'   facet_wrap(~abbreviation, scale="free_y")
#' }
#' @export
forecast_ili <- function(ilidat, horizon=4L, trim_date=NULL, models=list(arima="PDQ(0,0,0)+pdq(1:2,0:2,0)")) {

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

  ili_fit_for <- ts_fit_forecast(ilidat_tsibble,
                                 outcome="ili",
                                 horizon=horizon,
                                 models=models,
                                 trim_date=NULL,
                                 covariates=NULL,
                                 ensemble=FALSE)

  # extract the fit
  ili_fit <- ili_fit_for$tsfit

  # Get arima params if fitting an arima model
  if ("arima" %in% names(models)) {
    arima_params <-
      ili_fit %>%
      dplyr::mutate(x=purrr::map(arima, ~purrr::pluck(., "fit") %>% purrr::pluck("spec"))) %>%
      tidyr::unnest_wider(col=x) %>%
      dplyr::select(-arima)
  } else {
    arima_params <- NULL
  }

  # Get the forecast
  ili_forecast <- ili_fit_for$tsfor

  # Get the next #horizon weeks in a tibble
  ili_future <- ili_forecast %>%
    tibble::as_tibble() %>%
    dplyr::mutate(epiyear=lubridate::epiyear(yweek)) %>%
    dplyr::mutate(epiweek=lubridate::epiweek(yweek)) %>%
    dplyr::select(location, epiyear, epiweek, ili=.mean)

  # bound future cases at zero
  ili_future$ili[ili_future$ili<0] <- 0

  # bind the historical data to the new data
  ili_bound <- dplyr::bind_rows(ilidat %>% dplyr::mutate(forecasted=FALSE),
                                ili_future %>% dplyr::mutate(forecasted=TRUE)) %>%
    dplyr::arrange(location, epiyear, epiweek) %>%
    dplyr::inner_join(locations, by="location")

  # Create results
  res <- tibble::lst(ilidat, ilidat_tsibble, ili_fit, ili_forecast, ili_future, ili_bound, arima_params, locstats, removed)
  return(res)
}


#' Nowcast clinical laboratory percent positive flu data
#'
#' @description This function provides a naive nowcasting method for clinical laboratory percent positive flu data. The methodology simply averages the last 4 weeks of available data and uses this average as the value for the number of weeks specified to replace. This is useful given that there is reporting lag in the NREVSS clinical laboratory percent positive flu data.
#'
#' @param clin Data prepared with [get_cdc_clin]
#' @param weeks_to_replace Number of retrospective weeks to replace with nowcast; default is `1`
#'
#' @return A tibble formatted the same as that returned with `get_cdc_clin()` but where the n most recent weeks (n="weeks_to_replace") have been nowcasted.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## get data for Texas
#' tx_clin <-
#' get_cdc_clin(region = "state") %>%
#' dplyr::filter(location == "48")
#'
#' ## look at most recent observations
#' tx_clin %>%
#' dplyr::arrange(week_start) %>%
#' tail()
#'
#' ## now augment with default 1 week nowcast
#' tx_clin %>%
#' clin_nowcast(., weeks_to_replace = 1) %>%
#' dplyr::arrange(week_start) %>%
#' tail()
#'
#' ## and again augmented with 2 week nowcast instead
#' tx_clin %>%
#'  clin_nowcast(., weeks_to_replace = 2) %>%
#'  dplyr::arrange(week_start) %>%
#'  tail()
#'
#' }
#'
clin_nowcast <- function(clin, weeks_to_replace = 1) {

  ## get one week ahead of
  ahead_clin <-
    clin %>%
    dplyr::select(location, abbreviation, week_start,total,n_positive) %>%
    dplyr::group_by(location) %>%
    ## get last 4 weeks of data available to average
    dplyr::filter(dplyr::row_number() >= dplyr::n() - 3) %>%
    ## get mean of last 4 weeks of data by region
    dplyr::summarise(
      n_positive = mean(n_positive, na.rm = TRUE),
      total = mean(total, na.rm = TRUE),
      p_positive = n_positive/total * 100,
      ## this expression will advance dates ...
      ## ... creates a vector as long as the value of weeks to replace
      ## adds 7 to the last date for each week to replace
      week_start = max(week_start) + 7*1:weeks_to_replace,
      abbreviation = dplyr::first(abbreviation),
      .groups = "drop") %>%
    dplyr::mutate(
      epiweek = lubridate::epiweek(week_start),
      epiyear = lubridate::epiyear(week_start)) %>%
    dplyr::ungroup()

  dplyr::bind_rows(clin, ahead_clin) %>%
    dplyr::arrange(location, week_start)
}

#' Simple Poisson count forecaster
#'
#' @description This function is a helper that forecasts Poisson counts for 4 near-term horizons based on characteristics of recently observed count data. The function effectively takes a rolling average of last 4 observations (augmenting with each forecasted horizon as the horizons progress), then uses this average as the parameter for Lambda in a random draw from a Poisson distribution.
#'
#' @param .data Data frame with incoming data that includes a variable with counts (see ".var" argument), and location (must be stored in a column called "location") and a variable for sorting by date (must be stored in a column called "week_start")
#' @param .location The name of the location of interest
#' @param .var Bare, unquoted name of the variable with counts to be forecasted
#'
#' @return Vector of length 4 with Poisson forecasts for 4 horizons ahead.
#' @export
#'
#' @examples
#' \dontrun{
#' all_clin <- get_cdc_clin()
#' va_ahead <-
#'   dplyr::tibble(
#'     n_positive = pois_forc(all_clin, .location = "51", n_positive),
#'     total = pois_forc(all_clin, .location = "51", total),
#'     p_positive = n_positive / total)
#' va_ahead
#' }
pois_forc <- function(.data, .location, .var) {

  ## handle for NSE
  tmp_var <- dplyr::enquo(.var)

  last4 <-
    .data %>%
    ## ensures no duplicate
    dplyr::distinct_all() %>%
    dplyr::filter(location == .location) %>%
    dplyr::arrange(week_start) %>%
    utils::tail(4) %>%
    dplyr::pull(!!tmp_var)

  ## draw 1 random poisson value based on lambda computed by average of last 4 weeks of count data
  n1ahead <- stats::rpois(1, mean(last4))
  n2ahead <- stats::rpois(1, mean(c(last4[2:4],n1ahead)))
  n3ahead <- stats::rpois(1, mean(c(last4[3:4],n1ahead,n2ahead)))
  n4ahead <- stats::rpois(1, mean(c(last4[4],n1ahead,n2ahead,n3ahead)))

  c(n1ahead,n2ahead,n3ahead,n4ahead)
}

#' Forecast categorical targets
#'
#' @description This function takes probabilistic flu hospitalization forecast input and converts the forecasted values for each location to a categorical "change" indicator. The criteria for each level ("large decrease", "decrease", "stable", "increase", "large increase") was defined by the CDC (see link in references). The algorithm evaluates absolute changes in counts and rates (per 100k individuals) for the most recently observed week and a 2 week ahead forecasted horizon. This procedure runs independently for each location, and results in a formatted tabular output that includes each possible level and its corresponding probability of being observed (calculated from probabilistic quantiles) for every location.
#'
#' @param .forecast A tibble with "submission-ready" probabilistic flu hospitalization forecast data (i.e., tibble contained in list element returned from [format_for_submission])
#' @param .observed A tibble with observed flu admission data (i.e., tibble output from [prep_hdgov_hosp])
#'
#' @return Tibble with formatted categorical forecasts that includes the following columns:
#' - **forecast_date**: Date of forecast
#' - **target**: Name of target forecasted; fixed at "2 wk flu hosp rate change"
#' - **location**: FIPS code for the location
#' - **type**: The type of forecast output; fixed at "category"
#' - **type_id**: Categorical label; one of "large decrease", "decrease", "stable", "increase", "large increase"
#' - **value**: Probability of observing the given "type_id" at the given "location"
#'
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-experimental/README.md>
#' @export
#'
#' @examples
#' \dontrun{
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' prepped_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#'
#' # Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
#' hosp_fitfor <- ts_fit_forecast(prepped_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' prepped_forecast <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
#' forecast_categorical(prepped_forecast$ensemble, prepped_hosp)
#' }
forecast_categorical <- function(.forecast, .observed) {

  ## prep the .forecast object for experimental target summary
  forc4exp <-
    .forecast %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(quantile = as.numeric(quantile)) %>%
    ## only looking at 2 week ahead for now
    dplyr::filter(target == "2 wk ahead inc flu hosp") %>%
    ## join to internal locations object that has population data
    dplyr::left_join(locations, by = "location") %>%
    ## calculate rate per 100k
    dplyr::mutate(rate = (value/population)*100000) %>%
    ## exclude point estimates
    dplyr::filter(type == "quantile") %>%
    ## get columns of interest
    dplyr::select(forecast_date, location, quantile, value, rate)

  hosp4exp <-
    .observed %>%
    ## find observed data that is prior to the 1 week ahead forecast
    dplyr::filter(week_end == min(.forecast$target_end_date) - 7) %>%
    ## join to internal locations object that has population data
    dplyr::left_join(locations, by = "location") %>%
    ## calculate rate per 100k
    dplyr::mutate(lag_rate = (flu.admits/population)*100000) %>%
    ## get columns of interest
    dplyr::select(location, lag_value = flu.admits, lag_rate)

  ## get "probability range" from each quantile ...
  ## for example: 0.99 and 0.01 quantiles have same prob value (0.01)
  quants <-
    forc4exp %>%
    dplyr::filter(quantile < 0.5) %>%
    dplyr::pull(quantile) %>%
    unique(.)

  quant_denom <-
    c(quants,quants,0.5) %>%
    sum(.)

  ## join prepped forecast and prepped observed
  res <-
    dplyr::left_join(forc4exp,hosp4exp, by="location") %>%
    dplyr::left_join(rate_change, by="location") %>%
    ## calculate component indicators
    dplyr::mutate(
      ind_count = abs(value - lag_value),
      ind_rate = abs(rate - lag_rate),
      ind_rate2 = ifelse(rate - lag_rate > 0, "positive", "negative")
    ) %>%
    ## use component indicators to assess overall type per CDC flowchart
    dplyr::mutate(type_id =
                    dplyr::case_when(
                      ind_count < 20 | ind_count < count_rate1per100k ~ "stable",
                      (ind_count < 40 | ind_count < count_rate2per100k) & ind_rate2 == "positive" ~ "increase",
                      (ind_count < 40 | ind_count < count_rate2per100k) & ind_rate2 == "negative" ~ "decrease",
                      (ind_count >= 40 & ind_count >= count_rate2per100k) & ind_rate2 == "positive" ~ "large_increase",
                      (ind_count >= 40 & ind_count >= count_rate2per100k) & ind_rate2 == "negative" ~ "large_decrease"
                    )) %>%
    ## convert quantiles to "probability magnitude"
    dplyr::mutate(quantile = ifelse(quantile > 0.5, 1-quantile, quantile)) %>%
    dplyr::group_by(location,type_id) %>%
    ## sum up quantiles as probability magnitude over the total sum of quantiles
    dplyr::summarise(value = sum(quantile)/ (quant_denom), .groups = "drop") %>%
    ## fill in any missing type_ids in a given location with 0
    tidyr::complete(location,type_id, fill = list(value = 0)) %>%
    ## prep the submission format
    dplyr::mutate(forecast_date = unique(.forecast$forecast_date),
                  target = "2 wk flu hosp rate change",
                  type = "category") %>%
    dplyr::select(forecast_date, target,location, type, type_id, value)

  # What are the names of the categories to forecast?
  categories <- c("large_decrease", "decrease", "stable", "increase", "large_increase")
  # Which ones are missing from the data?
  missing_from_res <- categories[!categories %in% res$type_id]
  # Fill those in with zeros
  add_to_res <- tidyr::crossing(forecast_date=unique(res$forecast_date),
                                target=unique(res$target),
                                location=unique(res$location),
                                type=unique(res$type),
                                type_id=missing_from_res,
                                value=0)
  # Bind to the existing data, and make type_id a factor for possible plotting
  res <-
    res %>%
    dplyr::bind_rows(add_to_res) %>%
    dplyr::arrange(location) %>%
    dplyr::mutate(type_id=factor(type_id, levels=categories))

  res
}
