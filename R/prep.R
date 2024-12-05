#' @title Prep hospitalization data
#'
#' @description
#'
#' This function prepares hospitalization data retrieved using [get_hdgov_hosp] for downstream forecasting. The function optionally limits to states only, trims to a given date, removes incomplete weeks, and removes locations with little reporting over the last month.
#'
#' @param hdgov_hosp Daily hospital utilization data from [get_hdgov_hosp]
#' @param statesonly Logical as to whether or not to limit to US+DC+States only (i.e., drop territories); default is `TRUE`
#' @param trim Named list with elements for epiyear and epiweek corresponding to the minimum epidemiological week to retain; defaults to `list(epiyear=2020, epiweek=43)`, which is the first date of report in the healthdata.gov hospitalization data; if set to `NULL` the data will not be trimmed
#' @param remove_incomplete Logical as to whether or not to remove the last week if incomplete; defaults is `TRUE`.
#' @param min_per_week The minimum number of flu.admits per week needed to retain that state. Default removes states with less than 1 flu admission per week over the last 30 days.
#' @param augment Logical as to whether or not the data should be augmented with NHSN hospitalizations imputed backwards in time (see 'Details' for more); default is `FALSE`.
#' @param augment_stop Date at which the time series imputation data should stop; yyyy-mm-dd format; only used if "augment" is `TRUE` default is `"2020-10-18"`
#' @return A `tibble` with hospitalization data summarized to epiyear/epiweek with the following columns:
#'
#' - **abbreviation**: Abbreviation for the location
#' - **location**: FIPS code for the location
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **monday**: Date of Monday of the given epidemiological week
#' - **week_end**: Date of end (Saturday) of the given epidemiological week
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **flu.admits**: Count of flu cases among admitted patients on previous day
#' - **flu.admits.cov**: Coverage (number of hospitals reporting) for incident flu cases
#' - **ili_mean**: Estimate of historical ILI activity for the given epidemiological week
#' - **ili_rank**: Rank of the given epidemiological week in terms of ILI activity across season (1 being highest average activity)
#' - **hosp_mean**: Estimate of historical flu hospitalization rate for the given epidemiological week
#' - **hosp_rank**: Rank of the given epidemiological week in terms of flu hospitalizations across season (1 being highest average activity)
#'
#' @details
#'
#' The preparation for the weekly flu hospitalization data includes an option to "augment" the input time series. The augmentation is based on an extended time series that was developed with an imputation approach. The extended time series estimates flu hospitalizations at the state-level in years before NHSN reporting became available. If the user decides to include the imputed data, then the time series is extended backwards in time from the "augment_stop" date (defaults to October 18, 2020). The prepended data augmentation is formatted to match the true NSHN reporting. For more details on the data augmentation approach, refer to the publication: <https://www.medrxiv.org/content/10.1101/2024.07.31.24311314v1>.
#'
#' @export
#' @examples
#' \dontrun{
#' # Retrieve hospitalization data
#' hdgov_hosp <- get_hdgov_hosp(limitcols=TRUE)
#' # Prepare and summarize to weekly resolution
#' h <- prep_hdgov_hosp(hdgov_hosp)
#' h
#' }
prep_hdgov_hosp <- function(hdgov_hosp,
                            statesonly=TRUE,
                            trim=list(epiyear=2020, epiweek=43),
                            remove_incomplete=TRUE,
                            min_per_week=1,
                            augment = FALSE,
                            augment_stop = "2020-10-18") {
  # What's the last date you have data on? You'll need this to chop the data later on.
  last_date <- max(hdgov_hosp$date)

  # Summarize to epiyear, epiweek
  message("Summarizing to epiyear/epiweek")

  ## if the augment option is set then prepend with the imputed data
  if(augment) {
    hdgov_hosp <-
      nhsn_imputed %>%
      dplyr::select(state = location, date, flu.admits = mean_flu_admits) %>%
      ## NOTE: augmentation includes data that extends beyond the date we have used to trim HHS data previously
      ## this line (in combination with filter in the next line) ...
      ## ... will ensure that the data pulled from healthdata.gov is used for all dates after augment_stop date
      dplyr::filter(date <= as.Date(augment_stop)) %>%
      dplyr::bind_rows(dplyr::filter(hdgov_hosp, date > as.Date(augment_stop)),.) %>%
      dplyr::arrange(state, date)
  }

  hweek <- hdgov_hosp %>%
    dplyr::rename(location=state) %>%
    dplyr::mutate(epiyear=lubridate::epiyear(date), epiweek=lubridate::epiweek(date), .after=date) %>%
    dplyr::group_by(location, epiyear, epiweek) %>%
    dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop")

  # Summarize across states to get US data then bind back to states to get states+US.
  h <- hweek %>%
    dplyr::group_by(epiyear, epiweek) %>%
    dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), sum, na.rm=TRUE), .groups="drop") %>%
    dplyr::mutate(location = "US", .before = 1) %>%
    dplyr::bind_rows(hweek) %>%
    dplyr::arrange(epiyear, epiweek, location)

  # add back dates
  h <-
    h %>%
    dplyr::mutate(week_start=MMWRweek::MMWRweek2Date(epiyear, epiweek, 1),
                  monday=MMWRweek::MMWRweek2Date(epiyear, epiweek, 2),
                  week_end=MMWRweek::MMWRweek2Date(epiyear, epiweek, 7),
                  .before=epiyear)

  # Trim to when you actually have data
  if (!is.null(trim) && is.list(trim)) {
    message(sprintf("Trimming to %s", MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek)))
    h <- h %>% dplyr::filter(week_start >= MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek))
  }

  # Remove incomplete weeks
  if (remove_incomplete) {
    message(sprintf("Removing incomplete weeks (data prior to %s)", last_date))
    h <- h %>% dplyr::filter(week_end <= last_date)
  }

  # join in location codes
  h <- h %>%
    dplyr::rename(abbreviation=location) %>%
    dplyr::inner_join(locations, ., by="abbreviation") %>%
    dplyr::select(-location_name, -population)

  # Trim to US+states+DC only
  if (statesonly) {
    message("Filtering to US+DC+States only")
    h <-
      h %>%
      dplyr::filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0")))
  }

  # Drop states without minimum flu.admits per week
  if (is.numeric(min_per_week)) {
    message(sprintf("Removing states with < %d flu.admits per week on average over the last month", min_per_week))
    dropstates <-
      h %>%
      dplyr::filter(week_end >= last_date-30) %>%
      dplyr::group_by(abbreviation) %>%
      dplyr::summarize(dplyr::across(c(flu.admits, flu.admits.cov), mean, na.rm=TRUE)) %>%
      dplyr::arrange(flu.admits, flu.admits.cov) %>%
      dplyr::filter(flu.admits<min_per_week) %>%
      dplyr::pull(abbreviation) %>%
      unique()
    h <- h %>%
      dplyr::filter(!(abbreviation %in% dropstates))
    message(sprintf("Removed %d states: %s", length(dropstates), paste(dropstates, collapse=" ")))
  }

  # Join in the historical data
  h <-
    h %>%
    dplyr::inner_join(historical_severity, by="epiweek")

  return(h)
}


#' @title Make `tsibble`
#'
#' @description This function converts an input `tibble` with columns for [lubridate::epiyear] and [lubridate::epiweek]  into a [tsibble::tsibble] object. The `tsibble` has columns specifying indices for the time series as well as a date for the Monday of the epiyear/epiweek combination at each row.
#'
#' @param df A `tibble` containing columns `epiyear` and `epiweek`.
#' @param epiyear Unquoted variable name containing the MMWR epiyear.
#' @param epiweek Unquoted variable name containing the MMWR epiweek.
#' @param key Unquoted variable name containing the name of the column to be the tsibble key. See [tsibble::as_tsibble].
#' @return A `tsibble` containing additional columns `monday` indicating the date
#'   for the Monday of that epiweek, and `yweek` (a yearweek vctr class object)
#'   that indexes the `tsibble` in 1 week increments.
#' @examples
#' # Create an example tibble
#' d <- tibble::tibble(epiyear=c(2020, 2020, 2021, 2021),
#'                     epiweek=c(52, 53, 1, 2),
#'                     location="US",
#'                     somedata=101:104)
#' # Convert to tsibble (keyed time series tibble)
#' make_tsibble(d, epiyear = epiyear, epiweek=epiweek, key=location)
#' @export
make_tsibble <- function(df, epiyear, epiweek, key=location) {
  out <- df %>%
    # get the monday that starts the MMWRweek
    dplyr::mutate(monday=MMWRweek::MMWRweek2Date(MMWRyear={{epiyear}},
                                                 MMWRweek={{epiweek}},
                                                 MMWRday=2),
                  .after={{epiweek}}) %>%
    # convert represent as yearweek (see package?tsibble)
    dplyr::mutate(yweek=tsibble::yearweek(monday), .after="monday") %>%
    # convert to tsibble
    tsibble::as_tsibble(index=yweek, key={{key}})
  return(out)
}

#' @title Prepare NHSN weekly data
#'
#' @description This function prepares data retrieved from the weekly aggregated NHSN hospital respiratory data API. The data must be first retrieved using [prep_nhsn_weekly]. Once pulled from the API, this function will conditionally adjust partial reporting and add extended time series data (see 'Details' for more information). The preparation also includes joining to internal data prepared to estimate the historical severity of each epiweek.
#'
#' @param dat Weekly hospital utilization data from [prep_nhsn_weekly]
#' @param adjust_partial Logical as to whether or not the partial reporting should be adjusted (see 'Details' for more); default is `TRUE`.
#' @param trim Named list with elements for epiyear and epiweek corresponding to the minimum epidemiological week to retain; default is set to `NULL` the data will not be trimmed; to override the default use a named list (e.g., `list(epiyear=2020, epiweek=43)`)
#' @param statesonly Logical as to whether or not the data should be limited to states and DC (i.e., no other territories included); default is `TRUE`.
#' @param augment Logical as to whether or not the data should be augmented with NHSN hospitalizations imputed backwards in time (see 'Details' for more); default is `FALSE`.
#' @param augment_stop Date at which the time series imputation data should stop; yyyy-mm-dd format; only used if "augment" is `TRUE` default is `"2020-10-18"`
#'
#' @details
#'
#' The weekly aggregated data from NHSN includes locations that may have incomplete coverage of hospitals reporting (see <https://data.cdc.gov/Public-Health-Surveillance/Weekly-Hospital-Respiratory-Data-HRD-Metrics-by-Ju/mpgq-jmmr/about_data> for more information). The preparation in this function includes an optional step triggered by the "adjust_partial" argument to find the maximum coverage at any time point for each location, then adjusts the reported counts by a factor of X / Y_t, where X is the maximum coverage and Y_t is the coverage at time point t. If the coverage for the given week is near or equal to the maximum observed coverage, then the counts will have little to no effect on the counts. Note that this should be used with caution, as it is possible that some locations may have non-uniform reporting behaviors, especially during non-mandatory NHSN reporting windows. In other words, the counts may be adjusted using reported values from healthcare facilities that may be of a different size, serve different communities, or otherwise have different characteristics than the facilities that did not report.
#'
#' The preparation for the weekly flu hospitalization data includes an option to "augment" the input time series. The augmentation is based on an extended time series that was developed with an imputation approach. The extended time series estimates flu hospitalizations at the state-level in years before NHSN reporting became available. If the user decides to include the imputed data, then the time series is extended backwards in time from the "augment_stop" date (defaults to October 18, 2020). The prepended data augmentation is formatted to match the true NSHN reporting. For more details on the data augmentation approach, refer to the publication: <https://www.medrxiv.org/content/10.1101/2024.07.31.24311314v1>.
#'
#' @return A `tibble` with hospitalization data summarized to epiyear/epiweek with the following columns:
#'
#' - **abbreviation**: Abbreviation for the location
#' - **location**: FIPS code for the location
#' - **week_start**: Date of beginning (Sunday) of the given epidemiological week
#' - **monday**: Date of Monday of the given epidemiological week
#' - **week_end**: Date of end (Saturday) of the given epidemiological week
#' - **epiyear**: Year of reporting (in epidemiological week calendar)
#' - **epiweek**: Week of reporting (in epidemiological week calendar)
#' - **flu.admits**: Count of flu cases among admitted patients on previous day
#' - **flu.admits.cov**: Coverage (number of hospitals reporting) for incident flu cases
#' - **ili_mean**: Estimate of historical ILI activity for the given epidemiological week
#' - **ili_rank**: Rank of the given epidemiological week in terms of ILI activity across season (1 being highest average activity)
#' - **hosp_mean**: Estimate of historical flu hospitalization rate for the given epidemiological week
#' - **hosp_rank**: Rank of the given epidemiological week in terms of flu hospitalizations across season (1 being highest average activity)
#'
prep_nhsn_weekly <- function(dat,
                             adjust_partial = TRUE,
                             trim = NULL,
                             statesonly = TRUE,
                             augment = FALSE,
                             augment_stop = "2020-10-18") {

  ## fix "USA" abbreviation to be consistent with fiphde:::locations "US"
  dat <-
    dat %>%
    dplyr::mutate(abbreviation = ifelse(abbreviation == "USA", "US", abbreviation))

  if(adjust_partial) {
    dat <-
      dat %>%
      dplyr::group_by(abbreviation) %>%
      ## find the max reporting percentage and adjust up to that
      dplyr::mutate(max_reporting = max(flu.admits.cov.perc, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ## TODO: double check this logic
      ## performs the adjustment
      dplyr::mutate(flu.admits = round((max_reporting/flu.admits.cov.perc)*flu.admits))
  } else {
    dat <-
      dat %>%
      dplyr::mutate(flu.admits = round(flu.admits))
  }

  ## if the augment option is set then prepend with the imputed data
  if(augment) {
    dat <-
      ## start with internal imputation object
      nhsn_imputed %>%
      dplyr::select(abbreviation = location, week_start = date, flu.admits = mean_flu_admits) %>%
      ## NOTE: augmentation includes data that extends beyond the date we have used to trim HHS data previously
      ## this line (in combination with filter in the next line) ...
      ## ... will ensure that the data pulled from the NHSN API is used for all dates after augment_stop date
      dplyr::mutate(week_end = week_start + 6) %>%
      dplyr::select(-week_start) %>%
      dplyr::filter(week_end <= as.Date(augment_stop)) %>%
      dplyr::bind_rows(dplyr::filter(dat, week_end > as.Date(augment_stop)),.) %>%
      dplyr::arrange(abbreviation, week_end)
  }


  dat <-
    dat %>%
    dplyr::select(abbreviation, flu.admits, week_end, flu.admits.cov) %>%
    dplyr::left_join(locations) %>%
    dplyr::mutate(epiyear=lubridate::epiyear(week_end), epiweek=lubridate::epiweek(week_end)) %>%
    dplyr::mutate(week_start = mmwr_week_to_date(epiyear, epiweek, 1)) %>%
    dplyr::mutate(monday = mmwr_week_to_date(epiyear, epiweek, 2)) %>%
    dplyr::left_join(historical_severity, by="epiweek")

  # Trim to US+states+DC only
  if(statesonly) {
    message("Filtering to US+DC+States only")
    dat <-
      dat %>%
      dplyr::filter(location %in% c("US", stringr::str_pad(1:56, width=2, pad="0")))
  }

  # Trim to desired start
  if (!is.null(trim) && is.list(trim)) {
    message(sprintf("Trimming data to start at %s", MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek)))
    dat <-
      dat %>%
      dplyr::filter(week_start >= MMWRweek::MMWRweek2Date(trim$epiyear, trim$epiweek))
  }

  dat %>%
    select(abbreviation, location, week_start, monday, week_end, epiyear, epiweek, flu.admits, flu.admits.cov, ili_mean, ili_rank, hosp_mean, hosp_rank)

}
