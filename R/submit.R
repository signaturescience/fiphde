#' Format forecasts for submission
#'
#' This function prepares influenza hospitalization forecasts in the format required for submission to FluSight.
#'
#' @param .forecasts Forecasts to be formatted for submission; if method is "ts" this should be forecasts from [ts_fit_forecast]; otherwise this must be a `tibble` with forecast output (e.g. output from [glm_forecast]) with a colum designating "location"
#' @param method Method for forecasting; default is "ts" which will trigger the use of [ts_format_for_submission] internally
#' @param .target Name of the target in the forecast. Default is `"wk ahead inc flu hosp"`.
#'
#' @return A named list of tibbles, one for each model, formatted for submission.
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>
#' @export
#'
#' @examples
#' \dontrun{
#' # Get raw data from healthdata.gov
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#' ## save(h_raw, file="~/Downloads/h_raw.rd")
#' ## load(file="~/Downloads/h_raw.rd")
#'
#' # Prep, and make a tsibble
#' prepped_hosp <- prep_hdgov_hosp(h_raw, statesonly=TRUE)
#' prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' # Limit to only Virginia and US
#' prepped_hosp_tsibble <-
#'   prepped_hosp_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#'
#' # Fit a model
#' hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' # format for submission
#' formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
#' formatted_list
#' }
format_for_submission <- function(.forecasts, method = "ts", .target="wk ahead inc flu hosp") {

  if(method == "ts") {
    res <- ts_format_for_submission(.forecasts)
  } else {
    res_tmp <-
      .forecasts %>%
      dplyr::arrange(epiyear,epiweek) %>%
      dplyr::group_by(epiyear,epiweek) %>%
      dplyr::mutate(horizon = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(target = paste(horizon, .target)) %>%
      dplyr::mutate(target_end_date = MMWRweek::MMWRweek2Date(epiyear,epiweek,7)) %>%
      dplyr::mutate(forecast_date = lubridate::today()) %>%
      dplyr::mutate(type = ifelse(is.na(quantile), "point", "quantile")) %>%
      dplyr::mutate(quantile = round(quantile,3)) %>%
      dplyr::mutate(quantile = stringr::str_pad(quantile,width = 5, pad = "0", side = "right")) %>%
      dplyr::mutate(value = as.character(value)) %>%
      dplyr::select(forecast_date,target,target_end_date,location,type,quantile,value) %>%
      ## call to distinct to get rid of duplicated 0.5 quantile
      ## BETTER WAY TO DO THIS ??
      dplyr::distinct()

    res <- list(res_tmp)
    names(res) <- method
  }
  return(res)
}

#' @title Format time series forecast
#' @description Format time series forecast for submission.
#' @details Uses quantiles `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)` in the built-in `fiphde:::q`, using an accessory table `fiphde:::quidk`. See `data-raw/generate-sysdata.R` for details.
#' @param tsfor The forecast from [ts_fit_forecast].
#' @param .target Name of the target in the forecast. Default is `"wk ahead inc flu hosp"`.
#' @param .counts Logical. Default `TRUE` indicates that the target outcome is a count, and should be rounded off at an integer.
#' @return A named list of tibbles, one for each model, formatted for submission.
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>
#' @export
#' @examples
#' \dontrun{
#' # Get raw data from healthdata.gov
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#'
#' # Prep, and make a tsibble
#' prepped_hosp <- prep_hdgov_hosp(h_raw, statesonly=TRUE)
#' prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' # Limit to only Virginia and US
#' prepped_hosp_tsibble <-
#'   prepped_hosp_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#'
#' # Fit a model
#' hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' # format for submission
#' formatted_list <- ts_format_for_submission(hosp_fitfor$tsfor)
#' formatted_list
#' }
ts_format_for_submission <- function (tsfor, .target="wk ahead inc flu hosp", .counts=TRUE) {

  # Get the point estimates
  point_estimates <-
    tsfor %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(quantile=NA_real_, .after=yweek) %>%
    dplyr::mutate(type="point") %>%
    dplyr::rename(value=.mean) %>%
    dplyr::select(.model, yweek, location, quantile, value, type) %>%
    dplyr::arrange(.model, yweek)

  # Create quantile table from distribution column in the forecast
  quantiles <-
    tsfor %>%
    fabletools::hilo(as.double(sort(unique(quidk$interval)))) %>%
    fabletools::unpack_hilo(dplyr::ends_with("%")) %>%
    tidyr::gather(key, value, dplyr::contains("%")) %>%
    dplyr::inner_join(quidk, by="key") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(.model, yweek, location, quantile, value, type="quantile") %>%
    dplyr::arrange(.model, yweek, quantile) %>%
    # .5 quantile comes through twice in the quidk
    dplyr::distinct()

  # bind them all together
  submission_list <-
    list(point_estimates, quantiles) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(.model:type) %>%
    dplyr::arrange(.model, location, type, quantile, yweek) %>%
    ## processing to get horizon N
    dplyr::group_by(yweek) %>%
    dplyr::mutate(N=dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = paste(N, .target)) %>%
    dplyr::select(-N) %>%
    # Fix dates: as_date(yweek) returns the MONDAY that starts that week. add 5 days to get the Saturday date.
    dplyr::mutate(target_end_date=lubridate::as_date(yweek)+lubridate::days(5)) %>%
    dplyr::mutate(forecast_date=lubridate::today()) %>%
    dplyr::select(.model, forecast_date, target, target_end_date, location, type, quantile, value) %>%
    # Bound at zero
    dplyr::mutate(value=ifelse(value<0, 0, value)) %>%
    ## round up for counts of people if using .counts=TRUE
    ## ifelse tries to recycle .counts or something weird here, so make a new column and get rid of it
    dplyr::mutate(.counts=.counts) %>%
    dplyr::mutate(value = ifelse(.counts, ceiling(value), value)) %>%
    dplyr::select(-.counts) %>%
    dplyr::mutate(quantile = stringr::str_pad(quantile,width = 5, pad = "0", side = "right")) %>%
    dplyr::mutate(value = as.character(value)) %>%
    # split by the type of model
    split(.$.model) %>%
    # remove the .model variable from each list item
    purrr::map(dplyr::select, -.model)
    ## fix duplicating quantile rows
    # purrr::map(dplyr::distinct)

  return(submission_list)

}

#' Validate forecast submission
#'
#' This function will take the prepped forecast data and run a series of tests to validate the format.
#'
#' @param subdat A `tibble` with submission ready forecasts prepped by and stored in output of [format_for_submission]
#'
#' @return Named list with elements for each test (including logical for whether or not test passed and message if failed) and an overall "valid" logical with `TRUE` if all tests passed an `FALSE` if at least one failed
#' @export
#'
#' @md
#'
#' @examples
#' \dontrun{
#' # Get raw data from healthdata.gov
#' h_raw <- get_hdgov_hosp(limitcols=TRUE)
#'
#' # Prep, and make a tsibble
#' prepped_hosp <- prep_hdgov_hosp(h_raw, statesonly=TRUE)
#' prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
#'                                      epiyear = epiyear,
#'                                      epiweek=epiweek,
#'                                      key=location)
#' # Limit to only Virginia and US
#' prepped_hosp_tsibble <-
#'   prepped_hosp_tsibble %>%
#'   dplyr::filter(location %in% c("US", "51"))
#'
#' # Fit a model
#' hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
#'                                horizon=4L,
#'                                outcome="flu.admits",
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' # format for submission
#' formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
#' validate_forecast(formatted_list$ets)
#' }
#'
validate_forecast <- function(subdat) {
  ## get the flusight "config" which presumably will be used by their automated test suite
  fsconfig <- jsonlite::read_json("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/project-config.json")

  ## create list to store validation results
  valres <-
    list(
      locations = NA,
      targets = NA,
      values = NA,
      quantiles = NA,
      point = NA,
      date_format = NA,
      end_sat = NA,
      expect_sat = NA
    )

  ## now step through each check

  ## locations must be in list of location names
  allowed_locs <- unlist(fsconfig$target_groups[[1]]$locations)
  tst_loc <- all(subdat$location %in% allowed_locs)

  if(tst_loc) {
    valres$locations <- list(valid = TRUE, msg = NULL)
  } else {
    valres$locations <- list(valid = FALSE, msg = "The submission includes invalid locations.")
  }

  ## targets must be in list of target names
  req_targets <-
    unlist(fsconfig$target_groups[[1]]$targets) %>%
    sort() %>%
    paste0(., collapse = ",")

  tst_targ <-
    subdat %>%
    dplyr::group_by(location) %>%
    dplyr::distinct(target, .keep_all = TRUE) %>%
    dplyr::summarise(targets = paste0(target, collapse = ","), .groups = "drop") %>%
    dplyr::mutate(ind = targets == req_targets) %>%
    dplyr::pull(ind) %>%
    all(.)

  if(tst_targ) {
    valres$targets <- list(valid = TRUE, msg = NULL)
  } else {
    valres$targets <- list(valid = FALSE, msg = "The submission includes invalid targets.")
  }


  ## value must be non-negative
  tst_values <- all(subdat$value >= 0)

  if(tst_values) {
    valres$values <- list(valid = TRUE, msg = NULL)
  } else {
    valres$values <- list(valid = FALSE, msg = "The submission includes negative values.")
  }

  ## quantiles must be in valid quantile
  req_quants <-
    unlist(fsconfig$target_groups[[1]]$quantiles) %>%
    sort() %>%
    stringr::str_pad(.,width = 5, pad = "0", side = "right") %>%
    paste0(., collapse = ",")

  tst_quants <-
    subdat %>%
    dplyr::filter(type == "quantile") %>%
    dplyr::group_by(location,target) %>%
    dplyr::arrange(quantile) %>%
    dplyr::summarise(quants = paste0(quantile, collapse = ","), .groups = "drop") %>%
    dplyr::mutate(ind = quants == req_quants) %>%
    dplyr::pull(ind) %>%
    all(.)

  if(tst_quants) {
    valres$quantiles <- list(valid = TRUE, msg = NULL)
  } else {
    valres$quantiles <- list(valid = FALSE, msg = "The submission does not include all required quantiles or includes invalid quantiles.")
  }

  ## point type must have NA value for quantile
  tst_point <-
    subdat %>%
    dplyr::filter(type == "point") %>%
    dplyr::pull(quantile) %>%
    is.na() %>%
    all()

  if(tst_point) {
    valres$point<- list(valid = TRUE, msg = NULL)
  } else {
    valres$point <- list(valid = FALSE, msg = "The submission includes something other than 'NA' for quantile of point estimates.")
  }

  ## date columns are in YYYY-mm-dd format
  ## doesnt need to be perfect
  ## just needs to make sure this is either 2021 or 2022 ...
  ## ... to check that the dates are formatted right
  tst_date_format <-
    all(c(lubridate::year(strptime(subdat$forecast_date, format = "%Y-%m-%d")) %in% c(2021,2022,2023),
          lubridate::year(strptime(subdat$target_end_date, format = "%Y-%m-%d")) %in% c(2021,2022,2023)))

  if(tst_date_format) {
    valres$date_format <- list(valid = TRUE, msg = NULL)
  } else {
    valres$date_format <- list(valid = FALSE, msg = "The submission includes dates which are not 2021, 2022, or 2023.")
  }

  ## end_date is a saturday
  tst_end_sat <-
    all(weekdays(subdat$target_end_date, abbreviate = FALSE) == "Saturday")

  if(tst_end_sat) {
    valres$end_sat <- list(valid = TRUE, msg = NULL)
  } else {
    valres$end_sat <- list(valid = FALSE, msg = "The submission target end dates column includes date(s) that are not Saturday.")
  }

  forecast_day <- unique(weekdays(subdat$forecast_date))

  ## check that only one forecast date is specified
  # length(forecast_day) == 1

  ## if forecast date is monday/sunday then expected end date is sat of week * horizon
  ## if forecast date is tuesday then expected end date is sat of week * 1 + horizon

  tst_expect_sat <-
    subdat %>%
    tidyr::separate(target, into = c("horizon", "drop"), sep = " wk ", remove = FALSE) %>%
    dplyr::select(-drop) %>%
    dplyr::mutate(forecast_day = forecast_day) %>%
    ## this subtraction of horizon - 1 will make sure that when we multiply by 7 days ...
    ## ... we dont add any days to the current week (if the forecast day is sun or mon)
    dplyr::mutate(horizon_days = ifelse(forecast_day %in% c("Sunday","Monday"), (as.numeric(horizon) - 1)*7, as.numeric(horizon) * 7)) %>%
    ## ceiling date will give you the sunday
    dplyr::mutate(expected_sat = lubridate::ceiling_date(forecast_date + horizon_days, "weeks")-1) %>%
    dplyr::mutate(ind = target_end_date == expected_sat) %>%
    dplyr::pull(ind) %>%
    all(.)

  if(tst_expect_sat) {
    valres$expect_sat <- list(valid = TRUE, msg = NULL)
  } else {
    valres$expect_sat <- list(valid = FALSE, msg = "The submission target end dates do not line up with expected Saturdays by horizon. Note if submission forecast date is not Sunday or Monday, then forecasts are assumed to to start the following week.")
  }

  ## are all conditions valid ??
  valres$valid <- all(purrr::map_lgl(valres, "valid"))
  valres$valid
  return(valres)
}
