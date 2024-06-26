#' @title Fit and forecast with time-series approaches
#'
#' @description
#'
#' This function allows the user to fit time series models and forecast values out to a specified horizon. Starting from a `tsibble` object (see [make_tsibble]), the function fits the models specified as a list in the "models" argument. The "Details" section provides more information on how to parameterize the models used. Note that if the input `tsibble` is "keyed" (e.g., grouped by location) then the procedure will fit and forecast independently for each grouping.
#'
#' @param prepped_tsibble A `tsibble` with data formatted via [make_tsibble]
#' @param outcome The outcome variable to model; default is `"flu.admits"`
#' @param horizon Number of weeks ahead to forecast
#' @param trim_date The date (YYYY-MM-DD) at which time series models should start fitting; default `"2021-01-01"`; if set to `NULL` the input data will not be trimmed (i.e., all data will be used to fit time series models)
#' @param models A list of right hand side formula contents for models you want to run; default is `list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)', ets='season(method="N")', nnetar=NULL)` which runs a constrained ARIMA, non-seasonal ETS, and ignores the NNETAR model; see "Details" for more information
#' @param covariates Logical. Should flu hospitalization-specific covariates that should be modeled with the time series? If so, historical hospitalization and ILI rank for each epidemiological week, brought in with [prep_hdgov_hosp], is added to the ARIMA model.
#' @param ensemble Logical as to whether or not the models should be ensembled (using mean); default `TRUE`
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
#' # Retrieve hospitalization data
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' # Prepare and summarize hospitalization data to weekly resolution
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' # Create a keyed time series tibble with only locations of interest
#' prepped_tsibble <- make_tsibble(prepped_hosp,
#'                                 epiyear = epiyear,
#'                                 epiweek=epiweek,
#'                                 key=location) %>%
#'   dplyr::filter(location %in% c("US", "51"))
#'
#' # Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
#' hospfor1 <- ts_fit_forecast(prepped_tsibble,
#'                             horizon=4L,
#'                             outcome="flu.admits",
#'                             covariates=TRUE)
#' # Run an unconstrained ARIMA, seasonal ETS, no NNETAR
#' hospfor2 <- ts_fit_forecast(prepped_tsibble,
#'                             horizon=4L,
#'                             outcome="flu.admits",
#'                             covariates=TRUE,
#'                             models=list(arima='PDQ() + pdq()',
#'                                         ets='season(method=c("A", "M", "N"), period="3 months")',
#'                                         nnetar=NULL))
#' # Run an unconstrained ARIMA, seasonal ETS, NNETAR
#' hospfor3 <- ts_fit_forecast(prepped_tsibble,
#'                             horizon=4L,
#'                             outcome="flu.admits",
#'                             covariates=TRUE,
#'                             models=list(arima='PDQ() + pdq()',
#'                                         ets='season(method=c("A", "M", "N"), period="3 months")',
#'                                         nnetar="AR(P=1)"))
#' }
ts_fit_forecast <- function(prepped_tsibble,
                            outcome="flu.admits",
                            horizon=4L,
                            trim_date="2021-01-01",
                            models=list(arima='PDQ(0, 0, 0) + pdq(1:2, 0:2, 0)',
                                        ets='season(method="N")',
                                        nnetar=NULL),
                            covariates=TRUE,
                            ensemble=TRUE) {

  # If covariates is NULL or FALSE, make covariates NULL.
  # If covariates is TRUE, make it covariates=c("hosp_rank", "ili_rank")
  if (is.null(covariates) || !covariates) {
    covariates <- NULL
  } else if (covariates) {
    covariates <- c("hosp_rank", "ili_rank")
  } else {
    stop("This shouldn't happen. Problem with covariates reassignment.")
  }

  # Make model names case-insensitive)
  names(models) <- tolower(names(models))

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

  # Ensemble the models
  # TODO: make this more flexible and not hard-coded in the future?
  if (ensemble & !is.null(models$arima) & !is.null(models$ets) & !is.null(models$nnetar)) {
    tsfit <-
      tsfit %>%
      dplyr::mutate(ensemble=(arima+ets+nnetar)/3)
  } else if (ensemble & !is.null(models$arima) & !is.null(models$ets)) {
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
#'
#' @description
#'
#' This function forecasts ILI up to a specified future horizon. The models used can be parameterized with a "models" argument (for more details see [ts_fit_forecast]). By default, the function will use an ARIMA approach to model all locations in the input historical ILI data and then use the fitted models to forecast out to each of the horizons.
#' @param ilidat Data returned from [get_cdc_ili]
#'
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
#' # Retrieve ILI data
#' ilidat <- get_cdc_ili(region = c("national", "state", "hhs"),
#'                       years = 2010:lubridate::year(lubridate::today()))
#'
#' # Using data only from march 2020 forward, for US only
#' ilidat_us <- ilidat %>% dplyr::filter(location=="US")
#' # Replace most recent week with nowcast data, and nowcast last week
#' ilidat_us <- ilidat_us %>% replace_ili_nowcast(weeks_to_replace=1)
#' ilifor_us <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01")
#' # Take a look at objects that come out ILI forecasting procedure
#' ilifor_us$ili_fit
#' ilifor_us$arima_params
#' ilifor_us$ili_forecast
#' head(ilifor_us$ili_bound)
#' tail(ilifor_us$ili_bound, 10)
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


#' @title Nowcast clinical laboratory percent positive flu data
#'
#' @description
#'
#' This function provides a naive nowcasting method for clinical laboratory percent positive flu data. The methodology simply averages the last 4 weeks of available data and uses this average as the value for the number of weeks specified to replace. The function will always add 1 additional week to the observed data and (optionally) replace the number of weeks specified in the "weeks_to_replace" argument. This is useful given that there is reporting lag in the NREVSS clinical laboratory percent positive flu data.
#'
#' @param clin Data prepared with [get_cdc_clin]
#' @param weeks_to_replace Number of retrospective weeks to replace with nowcast; default is `1`
#'
#' @return A `tibble` with the following columns:
#'
#' - **abbreviation**: Abbreviation for the location
#' - **location**: FIPS code for the location
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **p_positive**: Percentage of positive specimens
#' - **n_positive**: Total number of positive specimens
#' - **total**: Total number of specimens tested
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get data for Texas
#' tx_clin <-
#'   get_cdc_clin(region = "state") %>%
#'   dplyr::filter(location == "48")
#'
#' # Look at most recent observations
#' tx_clin %>%
#'   dplyr::arrange(week_start) %>%
#'   tail()
#'
#' # Now augment with default 1 week nowcast
#' tx_clin %>%
#'   clin_nowcast(., weeks_to_replace = 1) %>%
#'   dplyr::arrange(week_start) %>%
#'   tail()
#'
#' # And again augmented with 2 week nowcast instead
#' tx_clin %>%
#'   clin_nowcast(., weeks_to_replace = 2) %>%
#'   dplyr::arrange(week_start) %>%
#'   tail()
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

#' @title Simple Poisson count forecaster
#'
#' @description
#'
#' This function is a helper that forecasts Poisson counts for near-term horizons based on characteristics of recently observed count data. The function effectively takes a rolling average of most recent observations (augmenting with each forecasted horizon as the horizons progress), then uses this average as the parameter for Lambda in a random draw from a Poisson distribution.
#'
#' @param .data Data frame with incoming data that includes a variable with counts (see ".var" argument), and location (must be stored in a column called "location") and a variable for sorting by date (must be stored in a column called "week_start")
#' @param .location The name of the location of interest
#' @param .var Bare, unquoted name of the variable with counts to be forecasted
#' @param horizon The number of horizons ahead to forecast; must be one of `4` or `5`; default is `4`
#'
#' @return Vector with Poisson forecasts for the number of horizons specified.
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
pois_forc <- function(.data, .location, .var, horizon = 4) {

  ## handle for NSE
  tmp_var <- dplyr::enquo(.var)

  lastn <-
    .data %>%
    ## ensures no duplicate
    dplyr::distinct_all() %>%
    dplyr::filter(location == .location) %>%
    dplyr::arrange(week_start) %>%
    utils::tail(horizon) %>%
    dplyr::pull(!!tmp_var)

  if(horizon == 5) {
    ## draw 1 random poisson value based on lambda computed by average of last 5 weeks of count data
    n1ahead <- stats::rpois(1, mean(lastn))
    n2ahead <- stats::rpois(1, mean(c(lastn[2:horizon],n1ahead)))
    n3ahead <- stats::rpois(1, mean(c(lastn[3:horizon],n1ahead,n2ahead)))
    n4ahead <- stats::rpois(1, mean(c(lastn[4:horizon],n1ahead,n2ahead,n3ahead)))
    n5ahead <- stats::rpois(1, mean(c(lastn[horizon],n1ahead,n2ahead,n3ahead,n4ahead)))

    c(n1ahead,n2ahead,n3ahead,n4ahead,n5ahead)

  } else if (horizon == 4) {
    ## draw 1 random poisson value based on lambda computed by average of last 4 weeks of count data
    n1ahead <- stats::rpois(1, mean(lastn))
    n2ahead <- stats::rpois(1, mean(c(lastn[2:horizon],n1ahead)))
    n3ahead <- stats::rpois(1, mean(c(lastn[3:horizon],n1ahead,n2ahead)))
    n4ahead <- stats::rpois(1, mean(c(lastn[4:horizon],n1ahead,n2ahead,n3ahead)))

    c(n1ahead,n2ahead,n3ahead,n4ahead)
  } else {
    stop("The horizon argument must be set at 4 or 5")
  }
}

#' @title Forecast categorical targets
#'
#' @description
#'
#' This function takes probabilistic flu hospitalization forecast input and converts the forecasted values for each location to a categorical "change" indicator. The criteria for each level ("large decrease", "decrease", "stable", "increase", "large increase") was defined by the CDC (see link in references). The algorithm evaluates absolute changes in counts and rates (per 100k individuals) for the most recently observed week and a 2 week ahead forecasted horizon. This procedure runs independently for each location, and results in a formatted tabular output that includes each possible level and its corresponding probability of being observed (calculated from probabilistic quantiles) for every location.
#'
#' @param .forecast A tibble with "submission-ready" probabilistic flu hospitalization forecast data (i.e., tibble contained in list element returned from [format_for_submission])
#' @param .observed A tibble with observed flu admission data (i.e., tibble output from [prep_hdgov_hosp])
#' @param method The categorical forecasting method to use; must be one of `"density"` or `"interpolation"`; default is `"density"`
#' @param format The submission format to be used; must be one of `"hubverse"` or `"legacy"`; default is `"hubverse"`
#' @param horizon The number of horizons ahead to forecast; must be one of `4` or `5`; default is `4`

#'
#' @return A `tibble` with formatted categorical forecasts.
#'
#' If format is `"hubverse"` the tibble will have the following columns:
#'
#' - **reference_date**: Date of reference for forecast submission
#' - **horizon**: Horizon for the given forecast
#' - **target**: Name of forecasted target
#' - **target_end_date**: Last date of the forecasted target (e.g., Saturday of the given epidemiological week)
#' - **location**: Name or geographic identifier (e.g., FIPS code) for location for the given forecast
#' - **output_type**: Type of forecasted value (e.g., "quantile")
#' - **output_type_id**: The quantile for the forecasted value if output_type is "quantile"
#' - **value**: The forecasted value
#'
#' If format is `"legacy"` the tibble will have the following columns:
#'
#' - **forecast_date**: Date of forecast
#' - **target**: Horizon and name of forecasted target
#' - **target_end_date**: Last date of the forecasted target (e.g., Saturday of the given epidemiological week)
#' - **location**: Name or geographic identifier (e.g., FIPS code) for location for the given forecast
#' - **type**: One of either "point" or "quantile" for the forecasted value
#' - **quantile**: The quantile for the forecasted value; `NA` if "type" is `"point"`
#' - **value**: The forecasted value
#'
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-experimental/README.md>
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve hospitalization data
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' # Prepare and summarize hospitalization data to weekly resolution
#' prepped_hosp <- prep_hdgov_hosp(h_raw)
#' # Create a keyed time series tibble with only locations of interest
#' prepped_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#'
#' # Run with default constrained ARIMA, nonseasonal ETS, no NNETAR
#' hosp_fitfor <- ts_fit_forecast(prepped_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                covariates=TRUE)
#' # Prepare forecast for quantile submission format
#' forc <- format_for_submission(hosp_fitfor$tsfor, method = "ts", format = "legacy")
#' # Run categorical summary of quantiles for the time series ensemble
#' forecast_categorical(forc$ensemble, prepped_hosp, method = "interpolation", format = "legacy")
#' }
forecast_categorical <- function(.forecast, .observed, method = "density", format = "hubverse", horizon = 4) {

  if(method == "interpolation") {
    ## prep the .forecast object for experimental target summary

    if(format != "legacy") {
      stop("Currently the interpolation method only works with 'legacy' forecast format.")
    }

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
      dplyr::left_join(legacy_rate_change, by="location") %>%
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
    allcrossed <- tidyr::crossing(forecast_date=unique(res$forecast_date),
                                  target=unique(res$target),
                                  location=unique(res$location),
                                  type=unique(res$type),
                                  type_id=categories,
                                  value=0)
    res <- allcrossed %>%
      dplyr::anti_join(res, by=c("forecast_date", "target", "location", "type", "type_id")) %>%
      dplyr::bind_rows(res, .) %>%
      dplyr::arrange(location) %>%
      dplyr::mutate(type_id=factor(type_id, levels=categories)) %>%
      dplyr::filter(!is.na(type_id))

    return(res)

  } else if (method == "density") {

    if(format != "hubverse") {
      stop("Currently the desnity method only works with 'hubverse' forecast format.")
    }

    last_week <-
      .observed %>%
      dplyr::filter(week_end == max(.observed$week_end)) %>%
      dplyr::select(location, flu_admits = flu.admits)

    res <-
      .forecast %>%
      dplyr::left_join(hubverse_rate_change %>% dplyr::select(-population), by = "location") %>%
      dplyr::left_join(last_week, by = "location") %>%
      dplyr::group_split(location, horizon) %>%
      purrr::map(density_probs, n_horizons = horizon) %>%
      purrr::list_rbind() %>%
      dplyr::rename(reference_date = forecast_date, output_type_id = target_name, value = target_prob) %>%
      dplyr::mutate(output_type = "pmf") %>%
      dplyr::mutate(target = "wk flu hosp rate change") %>%
      dplyr::select(reference_date, horizon, target, target_end_date, location, output_type, output_type_id, value) %>%
      dplyr::mutate(output_type_id = as.character(output_type_id)) %>%
      dplyr::mutate(reference_date = this_saturday()) %>%
      dplyr::mutate(value = as.character(value)) %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      dplyr::group_by(location, horizon) %>%
      dplyr::mutate(value = round_preserve(value, digits = 3)) %>%
      dplyr::mutate(value = as.character(value)) %>%
      dplyr::ungroup()

    return(res)
  } else {
    stop("The method must be one of 'density' or 'interpolation'.")
  }

}

#' Calculate categorical probability density
#'
#' This unexported helper function is used to build a distribution from quantile forecasts and then calculate the probability density for the thresholds associated with each category forecasted: large increase, increase, stable, decrease, large decrease.
#'
#' @param df Data frame with forecasts and categorical thresholds joined
#' @param n_horizons Number of horizons ahead
#' @param ... Additional arguments passed to `distfromq::distfromq()`
#'
#' @return Data frame with probabilities for each rate change category
#'
#'
density_probs <- function(df, n_horizons = 5, ...){

  a <- list(...)
  if ("lower_tail_dist" %in% names(a)) {
    ltd <- a$lower_tail_dist
  } else {
    ltd <- "norm"
  }
  if ("upper_tail_dist" %in% names(a)) {
    utd <- a$upper_tail_dist
  } else {
    utd <- "norm"
  }
  p <- as.numeric(df$output_type_id)
  q <- as.numeric(df$value)
  obs_flu_admits <- unique(df$flu_admits)
  ## NOTE: need to subtract observed flu admits because we're looking at *change* in forecast
  q <- q - obs_flu_admits
  cdf <- distfromq::make_p_fn(ps = p, qs = q, ...)
  # Calculate the target category thresholds

  tmp_horizon <- unique(df$horizon)

  ## DEFINE LIST WITH BOUNDARIES HERE ...

  if(n_horizons == 5) {
    thresh <-
      switch (as.character(tmp_horizon),
              "-1" = list(t1 = max(c(10, as.numeric(unique(df$count_rate1)))),
                          t2 = max(c(10, as.numeric(unique(df$count_rate2))))
              ),
              "0" = list(t1 = max(c(10, as.numeric(unique(df$count_rate1)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate3))))
              ),
              "1" = list(t1 = max(c(10, as.numeric(unique(df$count_rate2)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate4))))
              ),
              "2" = list(t1 = max(c(10, as.numeric(unique(df$count_rate2p5)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate5))))
              ),
              "3" = list(t1 = max(c(10, as.numeric(unique(df$count_rate2p5)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate5))))
              )
      )
  } else if (n_horizons == 4) {
    thresh <-
      switch (as.character(tmp_horizon),
              "0" = list(t1 = max(c(10, as.numeric(unique(df$count_rate1)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate2))))
              ),
              "1" = list(t1 = max(c(10, as.numeric(unique(df$count_rate1)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate3))))
              ),
              "2" = list(t1 = max(c(10, as.numeric(unique(df$count_rate2)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate4))))
              ),
              "3" = list(t1 = max(c(10, as.numeric(unique(df$count_rate2p5)))),
                         t2 = max(c(10, as.numeric(unique(df$count_rate5))))
              )
      )
  } else {
    stop("The number of horizons must be 4 or 5 ...")
  }

  t1 <- thresh$t1
  t2 <- thresh$t2

  # Calculate the probabilities for each category
  ## cdf is outputting the left tail
  ## space between t1 and -t1 indicates probability of stable
  ## subtract cdf of -t1 to remove tail for decrease + large decrease
  p_s <- cdf(t1) - cdf(-1 * t1)      # Stable
  ## space between t2 and t1 is increase
  p_i <- cdf(t2) - cdf(t1)           # Increase
  ## everything to the right of t2 (get right tail from subtracting left tail from 1)
  p_li <- 1 - cdf(t2)                # Large increase
  ## space between -t1 and -t2
  p_d <- cdf(-1 * t1) - cdf(-1 * t2) # Decrease
  ## left tail of -t2
  p_ld <- cdf(-1 * t2)               # Large decrease
  ## adding strict check for probability sums to 1
  ## round in case there are signif digit discrepancies
  stopifnot(round(sum(p_s, p_i, p_li, p_d, p_ld), 5) == 1)

  # Construct the output dataframe
  out_tmp <- data.frame(
    forecast_date = rep(unique(df$reference_date),5),
    abbreviation = rep(unique(df$abbreviation),5),
    location = rep(unique(df$location),5),
    location_name = rep(unique(df$location_name),5),
    target_name = c("stable","increase","large_increase","decrease","large_decrease"),
    target_prob = c(p_s,p_i,p_li,p_d,p_ld),
    target_end_date = unique(df$target_end_date),
    horizon = tmp_horizon,
    lower_tail_dist = ltd,
    upper_tail_dist = utd
  )
  return(out_tmp)
}

