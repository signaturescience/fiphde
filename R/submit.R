#' Format forecasts for submission
#'
#' This function prepares influenza hospitalization forecasts in the format required for submission to FluSight.
#'
#' @param .forecasts Forecasts to be formatted for submission; if method is "ts" this should be forecasts from [ts_fit_forecast]; otherwise this must be a `tibble` with forecast output (e.g. output from [glm_forecast]) with a colum designating "location"
#' @param method Method for forecasting; default is "ts" which will trigger the use of [ts_format_for_submission] internally
#'
#' @return A named list of tibbles, one for each model, formatted for submission.
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>
#' @export
#'
#' @examples
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
#'                                constrained=TRUE,
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' # format for submission
#' formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
#' formatted_list
#' }
format_for_submission <- function(.forecasts, method = "ts") {

  if(method == "ts") {
    res <- ts_format_for_submission(.forecasts)
  } else {
    res_tmp <-
      .forecasts %>%
      dplyr::arrange(epiyear,epiweek) %>%
      dplyr::group_by(epiyear,epiweek) %>%
      dplyr::mutate(horizon = dplyr::cur_group_id()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(target = paste0(horizon, " wk ahead inc flu hosp")) %>%
      dplyr::mutate(target_end_date = MMWRweek::MMWRweek2Date(epiyear,epiweek,7)) %>%
      dplyr::mutate(forecast_date = lubridate::today()) %>%
      dplyr::mutate(type = ifelse(is.na(quantile), "point", "quantile")) %>%
      dplyr::mutate(quantile = round(quantile,3)) %>%
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
#' @return A named list of tibbles, one for each model, formatted for submission.
#' @references <https://github.com/cdcepi/Flusight-forecast-data/blob/master/data-forecasts/README.md>
#' @export
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
#'                                constrained=TRUE,
#'                                covariates=c("hosp_rank", "ili_rank"))
#'
#' # format for submission
#' formatted_list <- ts_format_for_submission(hosp_fitfor$tsfor)
#' formatted_list
#' }
ts_format_for_submission <- function (tsfor) {
  # Make the 0.5 quantile the means (point estimates). The quidk doesn't contain a
  # median hilo. You'll bind this to the other quantiles in the step below.
  q5 <-
    tsfor %>%
    tibble::as_tibble() %>%
    dplyr::transmute(.model, yweek, location, quantile=0.5, value=.mean, type="quantile") %>%
    dplyr::arrange(.model, yweek)

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
    dplyr::arrange(.model, yweek, quantile)

  # bind them all together
  submission_list <-
    list(quantiles, point_estimates, quantiles) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(.model:type) %>%
    dplyr::arrange(.model, location, type, quantile, yweek) %>%
    ## processing to get horizon N
    dplyr::group_by(yweek) %>%
    dplyr::mutate(N=dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target=sprintf("%d wk ahead inc flu hosp", N)) %>%
    dplyr::select(-N) %>%
    # Fix dates: as_date(yweek) returns the MONDAY that starts that week. add 5 days to get the Saturday date.
    dplyr::mutate(target_end_date=lubridate::as_date(yweek)+lubridate::days(5)) %>%
    dplyr::mutate(forecast_date=lubridate::today()) %>%
    dplyr::select(.model, forecast_date, target, target_end_date, location, type, quantile, value) %>%
    # split by the type of model
    split(.$.model) %>%
    # remove the .model variable from each list item
    purrr::map(dplyr::select, -.model) %>%
    ## round up for counts of people
    purrr::map(~dplyr::mutate(., value = ceiling(value))) %>%
    ## fix duplicating quantile rows
    purrr::map(dplyr::distinct)

  # Bound at zero
  submission_list <-
    submission_list %>%
    purrr::map(~dplyr::mutate(., value=ifelse(value<0, 0, value)))

  return(submission_list)

}
